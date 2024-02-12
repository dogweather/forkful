---
title:                "Parsing HTML"
aliases: - /en/bash/parsing-html.md
date:                  2024-01-20T15:29:51.232707-07:00
simple_title:         "Parsing HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML means sifting through the structure and content of an HTML file to extract information. Programmers do it to access data, manipulate content, or scrape websites.

## How to:

Bash isn't the go-to for parsing HTML, but it can be done with tools like `grep`, `awk`, `sed`, or external utilities like `lynx`. For robustness, we'll use `xmllint` from the `libxml2` package.

```bash
# Install xmllint if necessary
sudo apt-get install libxml2-utils

# Sample HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Sample Page</title>
</head>
<body>
  <h1>Hello, Bash!</h1>
  <p id="myPara">Bash can read me.</p>
</body>
</html>
EOF

# Parse the Title
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "The title is: $title"

# Extract Paragraph by ID
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "The paragraph content is: $para"
```

Output:
```
The title is: Sample Page
The paragraph content is: Bash can read me.
```

## Deep Dive

Back in the day, programmers used regex-based tools like `grep` to scan HTML, but that was clunky. HTML isn't regular—it's contextual. Traditional tools miss this and can be error-prone.

Alternatives? Plenty. Python with Beautiful Soup, PHP with DOMDocument, JavaScript with DOM parsers—languages with libraries designed to understand HTML's structure.

Using `xmllint` in bash scripts is solid for simple tasks. It understands XML, and by extension, XHTML. Regular HTML can be unpredictable, though. It doesn’t always follow XML’s strict rules. `xmllint` forces HTML into an XML model which works well for well-formed HTML but can stumble on messy stuff.

## See Also

- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): Demystifies HTML DOM.
- [MDN Web Docs - Parsing and serializing XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): For XML parsing principles that apply to XHTML.
- [Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): A Python library for HTML parsing.
- [libxml2 Documentation](http://xmlsoft.org/): Details on `xmllint` and related XML tools.
