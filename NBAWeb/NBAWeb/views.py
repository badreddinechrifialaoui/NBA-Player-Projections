from django.shortcuts import render
import pandas as pd
import os

def home(request):
    """Renders projections dashboard."""
    
    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    data_folder = os.path.join(base_dir, 'data_feed')
    
    context = {
        'title': 'NBA AI Projections',
        'year': 2026,
    }
    
    try:
        csv_path = os.path.join(data_folder, 'projections.csv')
        if os.path.exists(csv_path):
            df = pd.read_csv(csv_path)
            df = df.round(1)
            
            # Get unique matchups for dropdown
            seen_matchups = set()
            unique_matchups = []
            
            for _, row in df.iterrows():
                matchup = tuple(sorted([row['team_abbreviation'], row['opponent']]))
                if matchup not in seen_matchups:
                    seen_matchups.add(matchup)
                    unique_matchups.append(f"{row['team_abbreviation']} vs {row['opponent']}")
            
            context['projections'] = df.to_dict('records')
            context['games'] = unique_matchups
        else:
            context['error'] = "File not found. Please run the R script."
    except Exception as e:
        context['error'] = f"Error loading data: {e}"
    
    return render(request, 'index.html', context)