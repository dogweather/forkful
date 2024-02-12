---
title:                "Logging"
date:                  2024-02-03T17:50:06.227894-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logging"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/logging.md"
---

{{< edit_this_page >}}

## What & Why?

Logging in software development is the process of recording information about a program’s execution, designed to track its behavior and diagnose issues. Programmers implement logging to monitor software performance, debug errors, and ensure system security and compliance, making it an indispensable tool for application maintenance and analysis.

## How to:

In Go, logging can be implemented using the standard library package `log`. This package provides simple logging capabilities, such as writing to standard output or to files. Let's begin with a basic example of logging to standard output:

```go
package main

import (
	"log"
)

func main() {
	log.Println("This is a basic log entry.")
}
```

Output:
```
2009/11/10 23:00:00 This is a basic log entry.
```

The timestamp at the beginning of the log entry is automatically added by the `log` package. Next, let's explore how to log to a file instead of standard output:

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("This log entry goes to a file.")
}
```

Now, let's implement a more advanced use case: customizing the logging format. Go allows you to create a custom logger with `log.New()`:

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "CUSTOM LOG: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("This is a custom log message.")
}
```

Output:
```
CUSTOM LOG: 2009/11/10 23:00:00 main.go:11: This is a custom log message.
```

This example prefixes each log message with "CUSTOM LOG: " and includes the date, time, and source file location.

## Deep Dive

The Go standard library's `log` package is straightforward and sufficient for many applications, but it lacks some of the more sophisticated features found in third-party logging libraries, such as structured logging, log rotation, and level-based logging. Packages like `zap` and `logrus` offer these advanced features and are well-regarded in the Go community for their performance and flexibility.

Structured logging, for example, allows you to log data in a structured format (like JSON), which is especially useful for modern cloud-based applications where logs might be analyzed by various tools or services. `zap`, in particular, is known for its high performance and low allocation overhead, making it suitable for applications where speed and efficiency are critical.

Historically, logging in Go has evolved significantly since the language's inception. Early versions of Go provided the basic logging capabilities that we see in the `log` package. However, as the language grew in popularity and the complexity of applications written in Go increased, the community began to develop more sophisticated logging libraries to meet their needs. Today, while the standard `log` package remains a viable option for simple applications, many developers turn to these third-party solutions for more complex logging requirements.
