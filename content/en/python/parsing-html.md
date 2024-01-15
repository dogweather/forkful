---
title:                "Parsing html"
html_title:           "Python recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/parsing-html.md"
---

{{< edit_this_page >}}

## Why
HTML is the backbone of every website, but it's not always easy to extract information from it. That's where HTML parsing comes in! By parsing HTML, we can easily retrieve data and manipulate it for our own purposes, making web scraping and data extraction a breeze.

## How To
```Python
# Import the necessary libraries
import requests
from bs4 import BeautifulSoup

# Create a variable with the URL of the website you want to scrape
url = "https://www.example.com"

# Use the requests library to get the HTML content of the website
response = requests.get(url)

# Use BeautifulSoup to parse the HTML content
soup = BeautifulSoup(response.content, 'html.parser')

# Use BeautifulSoup's find_all method to find specific elements on the webpage
headlines = soup.find_all('h1') # This will find all <h1> tags on the webpage

# Loop through the found elements and print their text
for headline in headlines:
    print(headline.text)

# Output:
# Example Website Title

```

## Deep Dive
HTML parsing works by breaking down HTML documents into smaller and more manageable parts. These parts are known as elements and they are structured in a tree-like hierarchy. By using a library like BeautifulSoup, we can easily navigate through this hierarchy and access specific elements and their attributes.

One important thing to note is that every element in HTML has an opening and closing tag, with the content in between. In the example above, the <h1> tag is the opening tag and the </h1> tag is the closing tag. Other elements, such as <img>, <a>, and <p>, also have their own specific set of attributes that can be accessed through HTML parsing.

Besides finding specific elements on a webpage, HTML parsing also allows us to extract data from these elements. For example, we can retrieve the text between the opening and closing tags of an element or access its attributes like the "src" attribute of an <img> tag.

Overall, HTML parsing provides a powerful way to extract and manipulate data from HTML documents, making web scraping and data extraction a much simpler task.

## See Also
- [BeautifulSoup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Python Requests Documentation](https://docs.python-requests.org/en/latest/)
- [HTML Tutorial for Beginners](https://www.w3schools.com/html/)