---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:37:42.171806-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go \u09AD\u09BE\u09B7\u09BE\u09AF\
  \u09BC YAML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\u09A8\u09BE\u0995\u09C7 \u09AA\u09CD\
  \u09B0\u09A5\u09AE\u09C7 \u098F\u0995\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09C0 \u0987\u09AE\u09CD\u09AA\u09CB\u09B0\u09CD\u099F \u0995\
  \u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7 \u09AF\u09BE YAML \u09AA\u09BE\u09B0\u09CD\
  \u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u09B8\u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\
  \u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8\
  \ \u0995\u09B0\u09C7, \u0995\u09BE\u09B0\u09A3 Go \u098F\u09B0\u2026"
lastmod: '2024-03-17T18:47:43.498607-06:00'
model: gpt-4-0125-preview
summary: "Go \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC YAML \u09A8\u09BF\u09AF\u09BC\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\
  \u09A8\u09BE\u0995\u09C7 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09C0 \u0987\u09AE\u09CD\
  \u09AA\u09CB\u09B0\u09CD\u099F \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7 \u09AF\
  \u09BE YAML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u09B8\
  \u09BF\u09B0\u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09B8\
  \u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7, \u0995\u09BE\u09B0\u09A3 Go \u098F\
  \u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09C0 \u09B8\u09B0\u09BE\u09B8\
  \u09B0\u09BF YAML \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09AE\u09B0\u09CD\
  \u09A5\u09A8 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4\
  \ \u0995\u09B0\u09C7 \u09A8\u09BE\u0964 \u098F\u0987 \u0989\u09A6\u09CD\u09A6\u09C7\
  \u09B6\u09CD\u09AF\u09C7 \u09B8\u09AC\u099A\u09C7\u09AF\u09BC\u09C7 \u099C\u09A8\
  \u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\
  \u09C0 \u09B9\u09B2 \"gopkg.in/yaml.v3\"\u0964 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\
  \u09BE\u09B0 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09A8\u09BF\u09AE\u09CD\u09A8\
  \u09B0\u09C2\u09AA."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কিভাবে:
Go ভাষায় YAML নিয়ে কাজ করার জন্য, আপনাকে প্রথমে একটি লাইব্রেরী ইম্পোর্ট করতে হবে যা YAML পার্সিং এবং সিরিয়ালাইজেশন সমর্থন করে, কারণ Go এর স্ট্যান্ডার্ড লাইব্রেরী সরাসরি YAML এর জন্য সমর্থন অন্তর্ভুক্ত করে না। এই উদ্দেশ্যে সবচেয়ে জনপ্রিয় লাইব্রেরী হল "gopkg.in/yaml.v3"। শুরু করার পদ্ধতি নিম্নরূপ:

1. **YAML প্যাকেজ ইনস্টল করা:**

```bash
go get gopkg.in/yaml.v3
```

2. **YAML কে Go struct এ পার্স করা:**

প্রথমে, Go তে এমন একটি struct ডিফাইন করুন যা আপনার YAML ডাটার গঠনের সাথে মেলে।

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("User: %s\nPassword: %s\n", config.Database.User, config.Database.Password)
}
```

**নমুনা আউটপুট:**

```
User: admin
Password: secret
```

3. **একটি Go struct কে YAML এ সিরিয়ালাইজ করা:**

YAML এ ফেরত Go struct রুপান্তর করার উপায় নিম্নরূপ।

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      User:     "admin",
      Password: "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**নমুনা আউটপুট:**

```yaml
---
database:
  user: admin
  password: supersecret
```

## গভীর পর্যালোচনা:
সফটওয়্যার উন্নয়নে YAML ব্যবহার এর বৃদ্ধি পেয়েছে এর মানব-পড়াযোগ্য ফরম্যাটের কারণে, যা এটিকে কনফিগারেশন ফাইল, ডকুমেন্টেশন, বা ডাটা এক্সচেঞ্জ ফরম্যাটের জন্য আদর্শ পছন্দ করে তোলে। JSON, এর সমতুল্য, এর তুলনায়, YAML মন্তব্য, স্কেলার ধরণ এবং সম্পর্ক বৈশিষ্ট্য অফার করে, একটি সমৃদ্ধ ডাটা সিরিয়ালাইজেশন কাঠামো প্রদান করে। তবে, এর স্বাচ্ছন্দ্য এবং বৈশিষ্ট্য জটিল পার্সিং এর খরচে আসে, যা যত্ন সহকারে না হলে সম্ভাব্য নিরাপত্তা ঝুঁকি (যেমন, যেকোনো কোড এক্সিকিউশন) জন্মাতে পারে।

Go এর জন্য "gopkg.in/yaml.v3" লাইব্রেরী একটি দৃঢ় সমাধান হিসাবে YAML প্রসেসিং এর জন্য আছে, ব্যবহারের সহজতা এবং সম্পূর্ণ বৈশিষ্ট্য সমর্থনের মধ্যে একটি ভারসাম্য তৈরি করে। বর্তমান অবস্থায়, যখন বিশেষ প্রকল্প প্রয়োজনীয়তা বা ব্যক্তিগত পছন্দ নির্ভর করে "go-yaml/yaml" (যা "gopkg.in/yaml.v3" এর পেছনের লাইব্রেরী) এর মতো বিকল্পগুলি থাকলেও, সাধারণত নির্বাচিত সংস্করণটি নির্ভর করে। বিশাল ডাটা সেট বা পারফরম্যান্স-জরুরী অ্যাপ্লিকেশনের সাথে dealing করার সময়, প্রোগ্রামাররা পার্সিং সময় এবং মেমোরি ব্যয় হ্রাস করার জন্য JSON এর মতো সহজ ফর্ম্যাটের দিকে ঝোঁক হতে পারে। যাহোক, মানব পড়ার সুবিধা এবং ব্যবহার সহজতার জন্�
