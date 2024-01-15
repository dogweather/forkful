---
title:                "Html को खोजना"
html_title:           "C++: Html को खोजना"
simple_title:         "Html को खोजना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Kyun
HTML parshing se kya matlab hai aur isse kyu hume lena chahiye?

Parsing HTML, yaani web pages ko analyze karna, bahut zaroori hai kyun ki internet par har information aur content HTML format mein hota hai. Agar hum APIs, web scraping ya data extraction ka use karna chahte hai, toh hame HTML code ko parse karna padega. 

## Kaise Karein
```C++
// Sabse pehle, hum HTML ko parse karne ke liye "HTMLParser" library ko include karenge
#include <HTMLParser.h>

// Phir, hum ek instance banayenge "HTMLParser" class ka
HTMLParser parser;

// Ab hum input HTML code ko string ke roop mein store karenge
string html = "<html><body><div><h1>Hello World!</h1></div></body></html>";

// Next, hum "parse" function ka use karke HTML code ko parse karenge
parser.parse(html);

// Ab, hum apni HTML code se specific elements ko extract kar sakte hai
string title = parser.getElement("h1"); // Output: Hello World!

// Iske alawa, hum attributes bhi extract kar sakte hai, jaise ki:
string titleClass = parser.getElementAttribute("h1", "class"); // Output: None

// Yeh sirf ek basic example hai, aur HTMLParser library ke aur bhi functions hai jo hume HTML parsing mein madad karte hai. Aap inki documentation check kar sakte hai.

// Ek aur tareeka hai html parsing ka, jo ki "Regular Expressions" ka use karta hai
// Hum <title> tag ko extract karne ke liye yeh regex ka use karenge:
regex titleRegex("<title>(.*?)</title>");

// Phir, hum apni HTML code ke saath yeh regex use karenge:
smatch matches;
regex_search(html, matches, titleRegex);

// Ab, hum title ko output karenge:
cout << matches.str(1) << endl; // Output: Hello World!

```

## Gehrayi Mein Jao
HTML parsing ek intricate process ho sakta hai, kyun ki HTML code bahut sare tags, attributes aur nested elements ka combination hota hai. Kuch important libraries aur tools jo HTML parsing mein useful kaam karte hai, woh hai BeautifulSoup aur XPATH. Inme se BeautifulSoup ek powerful HTML parsing library hai jo Python mein available hai. Iske alawa, Chrome ya Firefox ke developer tools bhi useful hai HTML parsing ke liye.

## Dekho Bhi
- HTMLParser library: https://github.com/mozilla/HTMLParser
- BeautifulSoup library: https://www.crummy.com/software/BeautifulSoup/
- XPATH: https://www.w3schools.com/xml/xpath_intro.asp
- Web scraping tutorial: https://realpython.com/beautiful-soup-web-scraper-python/