---
title:                "Clojure: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

# למה
היכן שאנשים עושים תחבירת מילות אחרונות בלבד 1-2 משפטים שמסבירים *למה* מישהו יעסוק באיחוד מחרוזות.

## איך לעשות
בלוקי קוד "Clojure..." הצורה עם דוגמאות קוד ופלט דוגמאות תחת ```...```

```Clojure
(def first-name "יוסי")
(def last-name "לוי")
"שלום, שלומי! נהנת להכיר אותך?"
(str "שלום," first-name "!" "נהנת להכיר אותך?")

```

הפלט יהיה: "שלום, יוסי! נהנת להכיר אותך?"

```Clojure
(def car "פורד")
(def model "פייסטה")
(def year "2008")
(str "ברכות ל" car " " model " " year "אהבת הנהג החדש זה מהסרט שלך")

```

הפלט יהיה: "ברכות לפורד פייסטה 2008 אהבת הנהג החדש זה מהסרט שלך"

## Deep Dive
תמיד נראה כמו משימה פשוטה לחבר מחרוזות, אבל ישנם כמה דברים החשוב לזכור. ראשית, בשביל לחבר מחרוזות בצורה תקינה, יש להכניס אותן לתוך פונקציה "str". בנוסף, עלינו לזכור שכאשר חוברים מילות אחרונות, הן צריכות להיות מופרדות עם רווח אחד ביניהן. כמו כן, ניתן להשתמש בתווים נוספים כמו מספרים וסימנים, אך חשוב לזכור לשים אותם בתוך גרשיים כדי שהם לא יתבלבלו עם המחרוזת.

## ראה גם
- [מדריך קצר על פונקציית "str" ב-Clojure](https://clojure.org/guides/learn/functions#_str)
- [למה חבר מחרוזות נחשב לפעולה גנרית בפונקציות רב-שימושיות](https://clojure.org/reference/protocols#_generic_functions)
- [דיון על פונקציית "str" וטיפים נוספים לניהול מחרוזות ב-Clojure](https://stackoverflow.com/questions/24458058/how-do-i-concatenate-strings-in-clojure)