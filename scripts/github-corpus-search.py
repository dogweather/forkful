import os
import requests

def count_github_repositories(search_term: str):
    api_token = os.getenv('GITHUB_API_TOKEN')
    search_endpoint = 'https://api.github.com/search/repositories'

    # Prepare the headers and query parameters
    headers = {
        'Authorization': f'token {api_token}',
        'Accept': 'application/vnd.github.v3+json',
    }
    params = {'q': search_term}

    # Perform the search request
    response = requests.get(search_endpoint, headers=headers, params=params)

    # Check the response status
    if response.status_code == 200:
        data = response.json()
        total_count = data['total_count']
        print(f"Total repositories found: {total_count}")
    else:
        print(f"Failed to fetch data. HTTP Status Code: {response.status_code}")


# Example usage with a search term
search_term = 'topic:hebrew'
count_github_repositories(search_term)
