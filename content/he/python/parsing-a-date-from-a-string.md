---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

ניתוח תאריך ממחרוזת הוא התהליך שבו מנתחים את המידע במחרוזת לתאריך עיל או יעד. התוכניתים עושים את זה כדי ליישב את המידע של המחרוזת של התאריך לצורה נראית לעין ולאחד אותם למערכת החישוב. 

## איך לעשות:

```Python
from datetime import datetime

date_string = "21/12/2020"

date_object = datetime.strptime(date_string, "%d/%m/%Y")

print(date_object)
```

ערך מנותח:

```Python
2020-12-21 00:00:00
```

## צלילה עמוקה

(1) בהקשר ההיסטורי, Python מספק באופן טבעי דרך לנתח תאריכים ממילה מתאימה.
(2) חלופות לניתוח באמצעות strptime יכולות להיות החלפת המחרוזת באופן ידני, אך זו דרך ממושךת ואינה רלוונטית לתאריכים מורכבים.
(3) ניתוח strptime משתמש במופע של datetime על מנת לחזור לאובייקט datetime מהתאריך המחרוזת. 


## ראה גם

- [תיעוד Python לניתוח מידע תאריך](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior)
- [סקר שלה על ניתוח תאריך וזמן מחרוזות ב-Python](https://stackabuse.com/converting-strings-to-datetime-in-python/)
- [אתר המלך Python - מידע על datetime](https://www.programiz.com/python-programming/datetime/strptime)