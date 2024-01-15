---
title:                "קריאת ארגומנטים מפקודת השורת הפקודה"
html_title:           "Javascript: קריאת ארגומנטים מפקודת השורת הפקודה"
simple_title:         "קריאת ארגומנטים מפקודת השורת הפקודה"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

אנשים עשויים לרצות לקרוא על ארגומנטי פקודת הקו הדיגיטלי של ה- command line כי זה יעזור להם להבין טוב יותר את השפה של ג'אווסקריפט ולהשתמש בה בצורה ברורה ומדוייקת.

## כיצד לעשות זאת

תיחזקו את המקלדת ותתחילו לכתוב! כאן נראה כמה דוגמאות של קוד ג'אווסקריפט לקריאת ארגומנטים מהפקודת הקו הדיגיטלי.

```javascript
// דוגמא 1: קריאת ארגומנטים פשוטה
const args = process.argv.slice(2);
console.log(args);

// אם הרצתם את הקוד למטה עם הארגומנטים "hello world!"
// הפלט יהיה:
// ['hello', 'world!']

// דוגמא 2: קריאת ארגומנטים והמרתם למספרים
const args = process.argv.slice(2);
const numbers = args.map(arg => Number(arg));
console.log(numbers);

// אם הרצתם את הקוד למטה עם ארגומנטים "5" ו- "10"
// הפלט יהיה:
// [5, 10]
```

### העומק

כעת כשאתם מכירים את הבסיסים, אפשר לעשות עוד המון דברים מעניינים עם ארגומנטי פקודת הקו הדיגיטלי בג'אווסקריפט. לדוגמא, ניתן לשלב את הפקודה process.argv.slice עם מתודות אחרות כמו indexOf ו- includes כדי לבדוק אם מילה מסוימת מופיעה ברשימת הארגומנטים או כדי למצוא את המיקום של מילה מסוימת.

## ראו גם

- [מדריך לפקודת הקו הדיגיטלי של ה- command line בג'אווסקריפט](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-command-line)
- [עמוד התיעוד של Node.js על פקודת הקו הדיגיטלי](https://nodejs.org/docs/latest/api/process.html#process_process_argv)