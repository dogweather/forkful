---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:51.352255-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Go \u09A4\u09C7 XML \u09AA\u09BE\
  \u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `encoding/xml`\
  \ \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964 \u098F\u0987 \u09AA\u09CD\u09AF\
  \u09BE\u0995\u09C7\u099C\u099F\u09BF XML \u0995\u09C7 Go \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BE\u0995\u09CD\u099F\u09B8 \u098F \u0986\u09A8\u09AE\u09BE\u09B0\u09CD\
  \u09B6\u09BE\u09B2 (\u09AA\u09BE\u09B0\u09CD\u09B8) \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u09C0\u09AF\u09BC\
  \u2026"
lastmod: '2024-03-17T18:47:43.503121-06:00'
model: gpt-4-0125-preview
summary: "Go \u09A4\u09C7 XML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF `encoding/xml` \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\
  \u0964 \u098F\u0987 \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u099F\u09BF XML \u0995\
  \u09C7 Go \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\u09CD\u099F\u09B8 \u098F \u0986\
  \u09A8\u09AE\u09BE\u09B0\u09CD\u09B6\u09BE\u09B2 (\u09AA\u09BE\u09B0\u09CD\u09B8\
  ) \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09AA\u09CD\u09B0\u09AF\u09BC\
  \u09CB\u099C\u09A8\u09C0\u09AF\u09BC \u09B8\u09B0\u099E\u09CD\u099C\u09BE\u09AE\
  \ \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u09AF\u09C7\u09AE\
  \u09A8, \u09A8\u09BF\u09AE\u09CD\u09A8\u09B2\u09BF\u0996\u09BF\u09A4 XML \u09A1\u09C7\
  \u099F\u09BE \u09AC\u09BF\u09AC\u09C7\u099A\u09A8\u09BE \u0995\u09B0\u09C1\u09A8\
  \ \u09AF\u09C7\u099F\u09BF \u098F\u0995\u099F\u09BF \u09AC\u0987 \u09AA\u09CD\u09B0\
  \u09A4\u09BF\u09A8\u09BF\u09A7\u09BF\u09A4\u09CD\u09AC \u0995\u09B0\u09C7."
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কিভাবে:


### Go তে XML পার্সিং
Go তে XML পার্স করার জন্য `encoding/xml` প্যাকেজ ব্যবহার করা হয়। এই প্যাকেজটি XML কে Go স্ট্রাক্টস এ আনমার্শাল (পার্স) করার জন্য প্রয়োজনীয় সরঞ্জাম প্রদান করে। যেমন, নিম্নলিখিত XML ডেটা বিবেচনা করুন যেটি একটি বই প্রতিনিধিত্ব করে:

```xml
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

এটা পার্স করতে, একটি struct ডিফাইন করুন যা XML স্ট্রাকচারের মিরর করবে:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

type Book struct {
    XMLName xml.Name `xml:"book"`
    ID      string   `xml:"id,attr"`
    Title   string   `xml:"title"`
    Author  string   `xml:"author"`
    Pages   int      `xml:"pages"`
}

func main() {
    data := []byte(`
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
`)

    var book Book
    err := xml.Unmarshal(data, &book)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Book: %+v\n", book)
}
```

আউটপুট:

```
Book: {XMLName:{Space: Local:book} ID:123 Title:Learning Go Author:John Doe Pages:359}
```

### Go তে XML জেনারেট করা
Go ডেটা স্ট্রাকচার থেকে XML নথি জেনারেট করতে, আপনি আবার `encoding/xml` প্যাকেজ ব্যবহার করেন। এই সময় আপনি Go structs কে XML এ মার্শাল করেন। পূর্ববর্তী `Book` struct গ্রহণ করে:

```go
package main

import (
    "encoding/xml"
    "fmt"
    "os"
)

func main() {
    book := &Book{
        ID:     "123",
        Title:  "Learning Go",
        Author: "John Doe",
        Pages:  359,
    }

    output, err := xml.MarshalIndent(book, "", "    ")
    if err != nil {
        panic(err)
    }

    fmt.Println(xml.Header + string(output))
}
```

আউটপুট:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<book id="123">
    <title>Learning Go</title>
    <author>John Doe</author>
    <pages>359</pages>
</book>
```

## গভীর ডাইভ
XML এর ভার্বোসিটি এবং জটিলতা অনেক অ্যাপ্লিকেশনের জন্য JSON এবং অন্যান্য ফরম্যাট গুলির জনপ্রিয়তা বৃদ্ধি করেছে। তবে, XML এর জটিল হায়ারার্কিকাল ডেটা প্রতিনিধিত্ব করার ক্ষমতা এবং এর ব্যাপক ব্যবহার পুরানো সিস্টেমগুলিতে এবং নির্দিষ্ট ডোমেইনগুলিতে (উদা., SOAP সার্ভিসেস) এর প্রাসঙ্গিকতা নিশ্চিত করে।

Go তে `encoding/xml` প্যাকেজটি XML নিয়ে কাজ করার জন্য শক্তিশালী পদ্ধতিগুলি প্রদান করে, তবে এর সীমাবদ্ধতাগুলি নোট করা জরুরি। যেমন, XML নেমস্পেসগুলি ব্যবহার করা জটিল হতে পারে এবং সরল ব্যবহারের ক্ষেত্রের চেয়ে XML স্পেসিফিকেশনের বিস্তারিত বোঝা প্রয়োজন হতে পারে। এছাড়াও, Go এর স্ট্যাটিক টাইপিং এবং `encoding/xml` প্যাকেজের মার্শালিং এবং আনমার্শালিং ক্ষমতা সাধারণত দক্ষ, কিন্তু গভীরভাবে নেস্টেড স্ট্রাকচার বা XML নথিগুলি নিয়ে কাজ করার সময় যা Go এর টাইপ সিস্টেমে সহজে ম্যাপ করা যায় না, তাতে ডেভেলপাররা চ্যালেঞ্জের সম্মুখীন হতে পারে।

অধিকাংশ আধুনিক অ্যাপ্লিকেশনের জন্য, JSON এর মতো বিকল্পগুলি সহজ এবং আরও দক্ষ। তবে, যখন XML এর মতো কন্টেক্স্টে কাজ করা প্রয়োজন— পুরানো সিস্টেমগুলি, নির্দিষ্ট শিল্প মানদণ্ড বা জটিল ডেটা প্রতিনিধিত্বের প্রয়োজনের জন্য— Go এর স্ট্যান্ডার্ড লাইব্রেরি কাজ সম্পাদনের জন্য দৃঢ় সরঞ্জাম প্রদান করে। যেমনটা সবসময় ঘটে থাকে, অ্যাপ্লিকেশন এবং পরিবেশের নির্দিষ্ট প্রয়োজনীয়তার উপর নির্ভর করে ডেটা ফরম্যাটের সেরা পছন্দ নির্ণীত হয়।
