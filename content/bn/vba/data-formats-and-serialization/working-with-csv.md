---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:42.426876-06:00
description: "CSV (Comma Separated Values) \u09AB\u09BE\u0987\u09B2 \u09A8\u09BF\u09AF\
  \u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\
  \u09AE\u09A8 \u09AA\u09CD\u09B2\u09C7\u0987\u09A8 \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09AA\u09A1\u09BC\u09BE\
  \ \u09AC\u09BE \u09B2\u09C7\u0996\u09BE\u09B0 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \ \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u09A1\u09C7\u099F\u09BE \u09AB\u09BF\u09B2\
  \u09CD\u09A1\u0997\u09C1\u09B2\u09BF \u0995\u09AE\u09BE \u09A6\u09CD\u09AC\u09BE\
  \u09B0\u09BE \u09AC\u09BF\u099A\u09CD\u099B\u09BF\u09A8\u09CD\u09A8\u0964 \u09AC\
  \u09BF\u09AD\u09BF\u09A8\u09CD\u09A8\u2026"
lastmod: '2024-03-17T18:47:43.882852-06:00'
model: gpt-4-0125-preview
summary: "CSV (Comma Separated Values) \u09AB\u09BE\u0987\u09B2 \u09A8\u09BF\u09AF\
  \u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u098F\
  \u09AE\u09A8 \u09AA\u09CD\u09B2\u09C7\u0987\u09A8 \u099F\u09C7\u0995\u09CD\u09B8\
  \u099F \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u09AA\u09A1\u09BC\u09BE\
  \ \u09AC\u09BE \u09B2\u09C7\u0996\u09BE\u09B0 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \ \u09AF\u09C7\u0996\u09BE\u09A8\u09C7 \u09A1\u09C7\u099F\u09BE \u09AB\u09BF\u09B2\
  \u09CD\u09A1\u0997\u09C1\u09B2\u09BF \u0995\u09AE\u09BE \u09A6\u09CD\u09AC\u09BE\
  \u09B0\u09BE \u09AC\u09BF\u099A\u09CD\u099B\u09BF\u09A8\u09CD\u09A8\u0964 \u09AC\
  \u09BF\u09AD\u09BF\u09A8\u09CD\u09A8\u2026"
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কি এবং কেন?

CSV (Comma Separated Values) ফাইল নিয়ে কাজ করা মানে এমন প্লেইন টেক্সট ফাইল থেকে পড়া বা লেখার কাজ করা যেখানে ডেটা ফিল্ডগুলি কমা দ্বারা বিচ্ছিন্ন। বিভিন্ন সফটওয়্যার অ্যাপ্লিকেশনের মধ্যে ডেটা বিনিময় সহজ করার জন্য প্রোগ্রামাররা প্রায়ই এই ধরনের কাজ করে থাকেন, বিভিন্ন প্রোগ্রামিং পরিবেশের মধ্যে CSV ফরম্যাটের সাধারণতা এবং ব্যাপক গ্রহণযোগ্যতার কারণে।

## কিভাবে:

ভিজ্যুয়াল বেসিক ফর অ্যাপ্লিকেশনস (VBA) বিল্ট-ইন ফাংশন এবং মেথডগুলির মাধ্যমে CSV ফাইলের সাথে কাজ করাকে সহজ করে। নিচে CSV ফাইলের সঙ্গে বেসিক অপারেশনগুলি নিয়ে কিছু উদাহরণ উপস্থাপিত হয়েছে।

### CSV ফাইল পড়া:

```basic
Sub ReadCSV()
    Dim filePath হিসাবে String
    filePath = "C:\example.csv"
    
    Open filePath For Input As #1
    
    Do Until EOF(1)
        Dim line হিসাবে String
        Line Input #1, line
        Dim dataFields() As String
        dataFields = Split(line, ",")
        
        'প্রয়োজন অনুসারে dataFields অ্যারে প্রক্রিয়া করুন
        Debug.Print Join(dataFields, ";") 'উদাহরণ আউটপুট দেখাচ্ছে কমা থেকে সেমিকোলনে রূপান্তর
    Loop
    
    Close #1
End Sub
```

### CSV ফাইলে লেখা:

```basic
Sub WriteCSV()
    Dim filePath হিসাবে String
    filePath = "C:\output.csv"
    Dim dataToWrite হিসাবে String
    dataToWrite = "ID,Name,Age" & vbCrLf & "1,John Doe,30" & vbCrLf & "2,Jane Doe,29"
    
    Open filePath For Output As #1
    Print #1, dataToWrite
    Close #1
End Sub
```

`output.csv` এ নমুনা আউটপুট:
```
ID,Name,Age
1,John Doe,30
2,Jane Doe,29
```

## গভীরে ডুব:

ঐতিহাসিকভাবে, CSV ফাইলগুলি টেবিলার ডেটা একটি টেক্সট ফরম্যাটে সংরক্ষণের একটি সরল পদ্ধতি হয়ে উঠেছে। এর গঠনের সাধারণতা, যেখানে প্রতি লাইনই একটি ডেটা রেকর্ডকে প্রতিনিধিত্ব করে এবং একটি রেকর্ডের প্রতিটি ক্ষেত্রই কমা দ্বারা বিচ্ছিন্ন, এটি CSV'র শক্তি এবং সীমাবদ্ধতা উভয়েরই। ফরম্যাটটি নেটিভভাবে ডাটা টাইপকে সমর্থন করে না, যার অর্থ সমস্ত ডাটা স্ট্রিং হিসেবে সংরক্ষিত হয়, এবং ডাটাকে সঠিক টাইপে রূপান্তর করার দায়িত্ব প্রোগ্রামারের উপর পড়ে।

ভিজ্যুয়াল বেসিক ফর অ্যাপ্�
