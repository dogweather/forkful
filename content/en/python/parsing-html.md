---
title:                "Parsing HTML"
date:                  2024-01-20T15:33:34.707300-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of taking HTML code and extracting information from it, sort of like finding needles in a haystack—if the haystack were made of tags and the needles were the data you want. Programmers do this to pull out data from websites, which can be anything from headlines on a news site to prices in an online store.

## How to:

Let's use Python to snatch some data from an HTML sample using the `BeautifulSoup` library, which makes parsing a breeze. If you haven't already, install the package using `pip install beautifulsoup4`.

```Python
from bs4 import BeautifulSoup

# Imagine this is your HTML
html_doc = """
<html>
<head>
    <title>The Dormouse's story</title>
</head>
<body>
    <p class="title">
        <b>The Dormouse's story</b>
    </p>
    <p class="story">Once upon a time there were three little sisters; and their names were
        <a href="http://example.com/elsie" class="sister" id="link1">Elsie</a>,
        <a href="http://example.com/lacie" class="sister" id="link2">Lacie</a> and
        <a href="http://example.com/tillie" class="sister" id="link3">Tillie</a>;
        and they lived at the bottom of a well.</p>
</body>
</html>
"""

# Soup it up
soup = BeautifulSoup(html_doc, 'html.parser')

# Find the title tag
title_tag = soup.title
print("Title of the story:", title_tag.string)

# Find all 'a' tags with the class 'sister'
sister_tags = soup.find_all('a', class_='sister')
print("Sisters' names and URLs:")
for sister in sister_tags:
    print(f"- Name: {sister.string}, URL: {sister['href']}")
```

Output will be:

```
Title of the story: The Dormouse's story
Sisters' names and URLs:
- Name: Elsie, URL: http://example.com/elsie
- Name: Lacie, URL: http://example.com/lacie
- Name: Tillie, URL: http://example.com/tillie
```

## Deep Dive

Back in the early days of the web, you'd parse HTML with regex and a lot of hope. This was messy because HTML isn't always neat and predictable. Enter libraries like BeautifulSoup, which navigates the tree structure of HTML, offering a gentle way to slice and dice the data.

There are also alternatives like `lxml` and `html.parser`, which BeautifulSoup itself can utilize as parsers. `lxml` is faster but less forgiving of bad HTML, while `html.parser` is slower but doesn't fuss over broken tags.

Under the hood, these libraries build a parse tree, turning tags into objects you can interact with. BeautifulSoup is like a friendly front-end to these parsers, translating your questions—like "What's the title?" or "Any links in here?"—into actions on the tree.

## See Also

- BeautifulSoup documentation: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- An introduction to parsing HTML with regex (and why you shouldn't do it): https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags
- Web scraping with Python (a practical guide): https://realpython.com/beautiful-soup-web-scraper-python/
