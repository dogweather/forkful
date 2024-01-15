---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## למה

כדי ליצור מיזמים חדשים ומגוונים ולהתחיל ללמוד ולשפר את הידע בתחום התכנות הפונקציונלי.

## איך להתחיל

עבור כל מיזם חדש, עלינו להתחיל עם הגדרת פרויקט בקוד קלוג'ר. למשל, ניתן ליצור קובץ קוד עם השם "myproject.clj" ולהתחיל עם קידוד ההגדרות הבאות:

```Clojure
(ns myproject.core
  (:require [clojure.string :as str]))

(defn hello-world []
  (str/join " " ["Hello" "World!"]))
  
(defn print-hello-world []
  (println (hello-world)))
```

כאן, אנו מגדירים את הפרויקט שלנו בשם "myproject" ומציינים כי נרצה להשתמש בספריית הקלודג'ר "string" על מנת להתאים עבורנו כמוווה את השפה. לאחר מכן, אנו מגדירים שתי פונקציות - אחת להדפסת "Hello World!" והשנייה לאחדן. לסיום הקוד, אנו מורידים את הפונקציה הנדרשת (במקרה זה,  "print-hello-world") ואנו מריצים אותה על ידי קידוד השורה התחתונה במנגנון קידוד הקלוג'ר.

תוכלו להריץ את הפונקצייה הנדרשת בטופס כזה:

```Clojure
(print-hello-world)
;; Hello World!
```

וזו הפלט המתקבל:

```
Hello World!
```

אם נרצה לכתוב את הפלט הנדרש לקובץ, נוכל להשתמש בפונקציה "spit" כדי להדפיס את התוכן לקובץ חדש:

```Clojure
(defn save-hello-world-to-file []
  (spit "hello.txt" (hello-world)))
```

ולאחר מכן, נוכל לקרוא את הקובץ החדש ולוודא כי הנתונים נשמרים בהתאם.

## העימות העמוק

כשמתחילים פרויקט חדש, יש מספר דברים שעלינו לקחת בחשבון. הנה מספר טיפים שיכולים לעזור לנו להתחיל:

- ניתן להשתמש בכליים שונים כ