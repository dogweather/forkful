---
title:                "Haskell: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

למה צריך להוריד עמוד אינטרנט? ייתכן שתרצו לעבוד עם מידע שנמצא על עמוד אינטרנט כדי ליצור אפליקציה, לבנות אתר או סתם לבדוק משהו מסוים.

## איך מבצעים

הנה דוגמא לקוד בשפת האסקל בכדי להוריד עמוד אינטרנט ולהציג את התוצאות:

```Haskell
import Network.HTTP
import Text.HTML.TagSoup

downloadWebPage :: String -> IO ()
downloadWebPage url = do
    response <- simpleHTTP (getRequest url)
    body <- getResponseBody response
    print $ parseTags body
```

תוצאה:

```
[TagOpen "html" [], TagOpen "head" [], TagOpen "title" [], TagText "Example Website", TagClose "title", TagOpen "body" [], TagOpen "header" [], TagOpen "h1" [], TagText "Welcome!", TagClose "h1", TagClose "header", ...]
```

## מעמקים

כדי להבין איך להוריד עמוד אינטרנט, ישנם מספר שלבים שחשוב לקחת בחשבון. למשל, יש לשלוט על מנגנוני צפייה של העמוד, וכן לזהות ולהפריד את התוכן שזכינו במערכת תגים. בנוסף, לנתח את הקוד של העמוד כדי להבין אילו אינפורמציות רלוונטיות ישנן ואיך הן מצויות בתוך הקוד.

## ראו גם

- [וידאו של הסבר ראשון להורדת עמוד אינטרנט בשפת האסקל](https://www.youtube.com/watch?v=vyTpmJ-yaJU)
- [כתבה עם דוגמאות לפרסום עמוד אינטרנט בעזרת אסקל](https://medium.com/@yehudkatz/building-a-simple-web-scraper-in-haskell-9bd4c593c989)
- [המשאבים המלאים לספריית HTTP בשפת אסקל](https://hackage.haskell.org/package/HTTP)