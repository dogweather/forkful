DOMAIN_NAME = 'forkful.ai'

with open('files') as file:
    for line in file.readlines():
        path     = line.strip()
        segments = path.split('/')
        filename = segments[4].removesuffix('.md')
        new_url  = f'https://{DOMAIN_NAME}/{segments[1]}/{segments[2]}/{segments[3]}/{filename}/'
        old_url  = f'https://{DOMAIN_NAME}/{segments[1]}/{segments[2]}/{filename}/'
        old_path = f'/{segments[1]}/{segments[2]}/{filename}/'

        print(f'["{old_path}","{new_url}"],')
