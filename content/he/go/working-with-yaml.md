---
title:                "עובדים עם yaml"
html_title:           "Go: עובדים עם yaml"
simple_title:         "עובדים עם yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
מתי אנו רוכשים דגש על YAML
כשפותחים את מערכות המידע שלנו כדי לקבל נתונים מגוונים מסוגים שונים, כמו מבני נתונים או ספריות הפכויות. את YAML נדרשת היכולת לקרוא ולכתוב נתונים מגוונים, כך שזה מקל עלינו לעבוד עם מגוון של נתונים שונים בקוד שלנו.

## How To
הנה דוגמה של קוד גו לקרוא ולכתוב YAML נתונים:

```Go
package main

import (
	"fmt"
	"io/ioutil"

	"gopkg.in/yaml.v3"
)

type Person struct {
	Name  string `yaml:"name"`
	Age   int    `yaml:"age"`
	Hobby string `yaml:"hobby"`
}

func main() {
	// קריאת קובץ YAML
	yamlFile, err := ioutil.ReadFile("person.yaml")
	if err != nil {
		fmt.Println("Error reading YAML file:", err)
		return
	}

	var person Person

	// ממיין את הנתונים ב-YAML לפי struct שלנו
	err = yaml.Unmarshal(yamlFile, &person)
	if err != nil {
		fmt.Println("Error unmarshaling YAML:", err)
		return
	}

	// הדפסת נתונים מה-YAML
	fmt.Printf("Name: %s\nAge: %d\nHobby: %s\n", person.Name, person.Age, person.Hobby)

	// כתיבת נתונים חדשים לקובץ YAML
	newPerson := Person{
		Name:  "John",
		Age:   30,
		Hobby: "coding",
	}

	yamlData, err := yaml.Marshal(newPerson)
	if err != nil {
		fmt.Println("Error marshaling YAML:", err)
		return
	}

	// כתיבת הנתונים לקובץ חדש
	err = ioutil.WriteFile("new_person.yaml", yamlData, 0644)
	if err != nil {
		fmt.Println("Error writing YAML file:", err)
		return
	}

}
```

כאן, אנו משתמשים בספרייה חיצונית של YAML לפירוש ולכתיבת נתונים בפורמט המתאים. את YAML files מנפתחים עם פתיחה של struct שמגדירה את הנתונים שאנו מעוניינים לקרוא/לכתוב.

## Deep Dive
ביתר פירוט, YAML הוא אקרונים של "YAML Ain't Markup Language", והוא מתאר פורמט נתונים קריא עבור אנשי בינוני-תקשורת. הוא פותח כחלופה ל-XML ול-JSON ומשתמש במרכיבי מציון ממוקד לאסוף את כל הנתונים בפורמט קל לקריאה ולעבודה עליהם. כמו כ