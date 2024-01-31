---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט נתונים הנקראו בקלות על ידי אנשים ותוכנות. תוכניתנים משתמשים בו לקונפיגורציה וחילוץ נתונים, משום שהוא פשוט ונוח.

## איך לעשות:
כדי לעבוד עם YAML ב-Go צריך להשתמש בספרייה חיצונית כמו `gopkg.in/yaml.v3`. הדוגמאות כאן מראות איך לעשות פרס ומארשלינג של YAML.

```Go
package main

import (
	"fmt"
	"log"

	"gopkg.in/yaml.v3"
)

type Config struct {
	Server string
	Port   int
}

func main() {
	data := `
server: localhost
port: 8080
`

	var config Config
	err := yaml.Unmarshal([]byte(data), &config)
	if err != nil {
		log.Fatalf("error: %v", err)
	}
	fmt.Printf("--- config:\nserver: %v\nport: %v\n\n", config.Server, config.Port)
}
```

תוצאה:

```
--- config:
server: localhost
port: 8080
```

## ניתוח עמוק:
YAML (Yet Another Markup Language או YAML Ain't Markup Language) פותח בהתחלת שנות ה-2000 כאלטרנטיבה הנקראת ל-XML. האלטרנטיבות כוללות JSON ו-TOML. YAML משמש בעיקר בוויזואליזציה של מבני נתונים וקונפיגורציות תוכנה. התמיכה של Go ב-YAML אינה מובנית, מכיוון שה-Marshal ו-Unmarshal מושגיפים מהסטנדרטים של Go.

## ראו גם:
- YAML ספריית Go: https://pkg.go.dev/gopkg.in/yaml.v3
- מועדון Golang ב-Stack Overflow: https://stackoverflow.com/questions/tagged/go
- מדריך YAML באנגלית: https://yaml.org/start.html
