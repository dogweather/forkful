---
title:                "Go: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

במאמר זה, נדבר על כתיבת קוד בשפת גו (Go) וכיצד ניתן להשתמש בקבצי YAML בתוכניות הגיוגריות שלנו. בין אם אתם מפתחים מתחילים או מומחים בשפת גו, יתרונות שימוש בפורמט קבצי YAML הם רבים. התאמתו הקלה לשמירה והפעלה של נתונים, ניתוח והשוואה בין קבצים, ולא פחות חשוב - הוודאות שהנתונים שלנו מוגנים ומגובים בצורה מדויקת. בכתיבה זו נציג לכם את הדרך לעבוד עם קבצי YAML בשפת גו.

## כיצד לעשות זאת

קוד גו

```Go
import (
	"fmt"
	"io/ioutil"
	"log"

	"gopkg.in/yaml.v2"
)

func main() {
	// קריאת קובץ YAML מתיקיית פרויקט
	yamlFile, err := ioutil.ReadFile("config.yaml")

	// בדיקת שגיאות בקריאת הקובץ
	if err != nil {
		log.Fatalf("לא ניתן לקרוא את הקובץ: %v", err)
	}

	// המרת הנתונים בקובץ למשתנה מטיפוס מאפיין יואם (map[string]interface{})
	var config map[string]interface{}
	err = yaml.Unmarshal(yamlFile, &config)

	// בדיקת שגיאות בהמרה
	if err != nil {
		log.Fatalf("לא ניתן להמיר נתונים מקבצי YAML: %v", err)
	}

	// הדפסת נתונים מהקובץ בפורמט JSON
	configJSON, err := json.Marshal(yamlFile)
	// בדיקת שגיאות בהמרה
	if err != nil {
		log.Fatalf("לא ניתן להמיר נתונים לפורמט JSON: %v", err)
	}
	// הדפסת הנתונים
	fmt.Println(string(configJSON))
}
```

פלט לדוגמה:

```Go
{"app":{"name":"MyApp","version":1.0,"log_location":"/var/log/myapp.log"},"database":{"host":"127.0.0.1","port":3306,"username":"myuser","password":"mypass"}}
```

ניתן כעת לקרוא את הנתונים מהמשתנה 'config' לפי שמו של המאפיין בעזרת הפונקציה 'config["property_name"]'.

## חפירה עמוקה

קבצי YAML מאפשרים לנו לארגן ולאחסן נתונים בפורמט תקין ונוח