import os
from time import sleep, time

import requests
from rich import print
import yaml



REPO_SEARCH_ENDPOINT = 'https://api.github.com/search/repositories'
API_TOKEN            = os.getenv('GITHUB_API_TOKEN')
if not API_TOKEN:
    raise ValueError("Please set the GITHUB_API_TOKEN environment variable")



def count_github_repositories(search_term: str) -> int:
    start_time = time()
    
    # Prepare the headers and query parameters
    headers = {
        'Authorization': f'token {API_TOKEN}',
        'Accept': 'application/vnd.github.v3+json',
    }
    params = {'q': search_term}

    # Perform the search request
    response = requests.get(REPO_SEARCH_ENDPOINT, headers=headers, params=params)

    # Check the response status
    if response.status_code != 200:
        raise ValueError(f"Failed to perform the search: {response.status_code}")

    data = response.json()
    
    # Ensure a 2 second delay between requests
    elapsed_time = time() - start_time
    sleep_time   = max(0, 2.1 - elapsed_time)
    sleep(sleep_time)  

    return data['total_count']


# Read datasets/a-language-list.yaml and perform the search for each item:
with open('datasets/a-language-list.yaml') as f:
    language_file = yaml.safe_load(f)

for language in language_file['languages']:
    search_term = f'topic:{language}'
    print(language, count_github_repositories(search_term))
