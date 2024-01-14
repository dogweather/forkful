---
title:                "Clojure: יצירת קובץ זמני"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

יתכן שכבר נתקלתם בצורך ליצור קובץ זמני בקלות תוך כדי תהליך התכנות שלכם. היות וקבצים זמניים משמשים בדרך כלל כחלק מהפתרונות לתרגילים ובדיקות מסוימות, יהיה מועיל לדעת כיצד ליצורם בקלות ובפשטות בשפת Clojure.

## איך ליצור קובץ זמני ב-Clojure

כדי ליצור קובץ זמני בשפת Clojure, נוכל להשתמש בפונקציה `with-open` ולהעביר לה את הכתובת של הקובץ שאנו רוצים ליצור. לדוגמה, ננסה ליצור קובץ זמני בשם "temp.txt":

```Clojure
(with-open [temp-file (java.io.File/createTempFile "temp" ".txt")]
  (println "קובץ זמני נוצר בהצלחה!"))
```

כאשר נריץ את הקוד הזה, נקבל את הפלט הבא במסך:

```Clojure
 >>>קובץ זמני נוצר בהצלחה!
```

ניתן גם להשתמש בפונקציה `with-open` כדי לכתוב לקובץ זמני, כך שהקובץ יימחק אוטומטית כשהקוד ייגמר. נוכל לעשות זאת מכיוון שהפונקציה מבצעת את פעולת הסגירה של הקובץ אוטומטית בסוף הבלוק שבתוך הפונקציה.

לדוגמה, ננסה לכתוב את הטקסט "היי עולם!" לקובץ זמני בשם "temp.txt":

```Clojure
(with-open [temp-file (java.io.File/createTempFile "temp" ".txt")]
  (spit temp-file "היי עולם!"))
```

כאשר נריץ את הקוד הזה, נקבל את הפלט הבא במסך:

```Clojure
>>> nil
```

ניתן גם לקרוא את התוכן של הקובץ הזמני באמצעות הפונקציה `slurp`:

```Clojure
(slurp temp-file)
```

והפלט שנקבל יהיה:

```Clojure
>>> "היי עולם!"
```

## חקירה מעמיקה

לכתוב ולקרוא קבצ