import os

import requests
from rich import print


REPO_SEARCH_ENDPOINT = 'https://api.github.com/search/repositories'
API_TOKEN            = os.getenv('GITHUB_API_TOKEN')
if not API_TOKEN:
    raise ValueError("Please set the GITHUB_API_TOKEN environment variable")


def count_github_repositories(search_term: str) -> int:
    # Prepare the headers and query parameters
    headers = {
        'Authorization': f'token {API_TOKEN}',
        'Accept': 'application/vnd.github.v3+json',
    }
    params = {'q': search_term}

    # Perform the search request
    response = requests.get(REPO_SEARCH_ENDPOINT, headers=headers, params=params)

    # Check the response status
    if response.status_code == 200:
        data = response.json()
        return data['total_count']
        print(f"Total repositories found: {total_count}")
    else:
        raise ValueError(f"Failed to perform the search: {response.status_code}")


# Example usage with a search term
search_term = 'topic:hebrew'
print(search_term, count_github_repositories(search_term))
