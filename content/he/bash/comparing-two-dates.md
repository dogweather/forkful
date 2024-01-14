---
title:    "Bash: השוואת שני תאריכים"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

נראה קל להשוות שתי תאריכים, אבל בפועל זה עשוי להיות מעט מורכב. לכן, זה מעשה טוב ללמוד כיצד לעשות זאת ב-Bash כדי להיות יעילים בכתיבת קוד.

## איך לעשות

כשמדובר בשפות תכנות, ישנן כמה דרכים להשוות שתי תאריכים. ב-Bash, ישנן כמה אפשרויות די פשוטות. הנה דוגמאות לקוד ותצוגה של התוצאות:

```Bash
# השוואת תאריך עם מבנה yyyy-mm-dd
date1="2020-01-15"
date2="2021-05-20"

if [ "$date1" == "$date2" ]; then
  echo "זהים"
elif [ "$date1" \< "$date2" ]; then
  echo "$date1 מוקדם מ-$date2"
else
  echo "$date2 מוקדם מ-$date1"
fi

# השוואת תאריך עם מבנה dd/mm/yy
date1="1/15/20"
date2="5/20/21"

# המרת התאריך לפורמט של Unix timestamp
date1_unix=$(date -d "$date1" "+%s")
date2_unix=$(date -d "$date2" "+%s")

if [ "$date1_unix" -eq "$date2_unix" ]; then
  echo "זהים"
elif [ "$date1_unix" -lt "$date2_unix" ]; then
  echo "$date1 מוקדם מ-$date2"
else
  echo "$date2 מוקדם מ-$date1"
fi
```

תוצאות הריצה:

```
2020-01-15 מוקדם מ-2021-05-20
1/15/20 מוקדם מ-5/20/21
```

## עיון מעמיק

בכדי להשוות שתי תאריכים ב-Bash, כדאי להשתמש בפקודת [date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html). חלק מהאופציות שמצוינות בדוגמאות הראשונות ניתן לעשות גם עם הפקודה הזו. לפני השתמשות ב-`date` כדאי לבדוק את הפורמט של התאריך באמצעות [Formats Date Strings Using GNU date](https://linuxmoz.com/formats-date-strings-using-gnu-date/) כדי לוודא שהוא תואם את הצורך שלכם.

בנוסף, אפשר גם להשתמש בפקודה [cmp](https://www.gnu.org/software/diffutils/manual/html_node/cmp-invocation.html) כדי להשוות שתי קבצי טקסט ולאחר מכן להשתמש בתוצאה של השוואת הקבצים כ