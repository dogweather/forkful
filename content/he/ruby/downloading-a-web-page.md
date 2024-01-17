---
title:                "הורדת דף אינטרנט"
html_title:           "Ruby: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
להוריד את הדף האינטרנטי הינו פעולה שבה מתקבל מידע מאתר אינטרנט ונשמר במחשב המשתמש. תהליך זה נעשה בכדי לשלוט על המידע ולאפשר למתכנתים לעבוד איתו ולהשתמש בו בקלות.

## איך לעשות:
להלן דוגמאות לקוד ופלט בתוך בלוקי קוד עם הסמל `Ruby ... `.

### דוגמה 1:
``` Ruby
require 'open-uri'
url = "https://www.example.com"
html_file = open(url).read
puts html_file
```

### פלט:
```
<!doctype html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <style type="text/css">
  body {
    background-color: #f0f0f2;
    margin: 0;
    padding: 0;
    font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
    
## נחישות לעומק:
כדי להוריד דף אינטרנט, מתכנתים יכולים להשתמש בספריית open-uri הכלולה בסטנדרט הקל להבנה והשימוש. יחד עם זאת, קיימות ספריות נוספות כגון HTTParty ו- httpclient שמאפשרות יצירת בקשות HTTP לכתובת אתר ולהחזיר את התוכן שלו. בנוסף, אפשר להשתמש בפקד Nokogiri כדי לשפר את תהליך החלוקה והניתוח של תוכן האינטרנט.

## ראו גם:
- [ספריית open-uri ברמת קלטת רובי](https://ruby-doc.org/stdlib-2.6.3/libdoc/open-uri/rdoc/OpenURI.html)
- [ספריית HTTParty](https://github.com/jnunemaker/httparty)
- [פקד Nokogiri](https://nokogiri.org/)