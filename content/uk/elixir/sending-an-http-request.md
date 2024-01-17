---
title:                "Надсилання http-запиту"
html_title:           "Elixir: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

Що & Чому?
Відправлення HTTP-запиту - це процес взаємодії з веб-сервером для отримання або відправлення даних. Програмісти використовують це для отримання інформації з інших джерел або для відправлення даних на сервер.

Як це зробити:
```Elixir
{:ok, response} = HTTPoison.get("https://www.example.com")
IO.puts response.body

# Вихідний код:
{"<!DOCTYPE html>\n<html>\n<head>\n<title>Example Domain</title>\n<meta charset=\"UTF-8\" />\n<meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\" />\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n<style type=\"text/css\">\nbody {\n background-color: #f0f0f2;\n margin: 0;\n padding: 0;\n font-family: \"Open Sans\", \"Helvetica Neue\", Helvetica, Arial, sans-serif;\n}\ndiv {\n width: 600px;\n margin: 5em auto;\n padding: 5px;\n}\na:link {\n color: #38488f;\n text-decoration: none;\n}\na:visited {\n color: #38488f;\n text-decoration: none;\n}\na:hover {\n color: #f0f0f2;\n background-color: #30a1d1;\n text-decoration: none;\n}\nid \"main-section\" {\n background-color: white;\n text-align: center;\n}\nid \"main-title\" {\n font-weight: bold;\n font-size: 200%;\n margin: 0px;\n}\nid \"description\" {\n text-align: left;\n font-weight: normal;\n color: #000000;\n font-size: 120%;\n}\n</style>\n</head>\n<body>\n<div id=\"main-section\">\n<h1 id=\"main-title\">Example Domain</h1>\n<p id=\"description\">This domain is for use in illustrative examples in documents. You may use this\n domain in literature without prior coordination or asking for permission.</p>\n<div id=\"main-section\"></div>\n</body>\n</html>\n", %{"Server" => "Golfe2.0/1.7.3", "Cache-Control" => "max-age=0, private, must-revalidate", "Strict-Transport-Security" => "max-age=31536000"}}
```

Глибокий аналіз:
Відправлення HTTP-запиту було створено для полегшення обміну даними між клієнтськими програмами і веб-серверами. Існують інші способи обміну даними, такі як FTP або SMTP, але HTTP зараз є найпоширенішим. У Elixir для відправлення HTTP-запиту можна використовувати різні бібліотеки, такі як HTTPoison, Finch або Tesla.

Дивіться також:
Подальшу інформацію про відправлення HTTP-запитів можна знайти на офіційному сайті Elixir (https://elixir-lang.org/) та на сторінках документації для бібліотек HTTPoison (https://hexdocs.pm/httpoison/HTTPoison.html), Finch (https://hexdocs.pm/finch/Finch.html), та Tesla (https://hexdocs.pm/tesla/Tesla.html).