---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:21.938408-06:00
description: "Visual Basic for Applications (VBA) \u098F \u09A8\u09BF\u09AF\u09BC\u09AE\
  \u09BF\u09A4 \u0985\u09AD\u09BF\u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF (regex)\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8, \u09AE\u09C7\u09B2\u09C7, \u098F\u09AC\u0982 \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF\
  \ \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u0989\u09AA\u09BE\u09AF\
  \u09BC \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.844191-06:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications (VBA) \u098F \u09A8\u09BF\u09AF\u09BC\u09AE\
  \u09BF\u09A4 \u0985\u09AD\u09BF\u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF (regex)\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8, \u09AE\u09C7\u09B2\u09C7, \u098F\u09AC\u0982 \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF\
  \ \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u0989\u09AA\u09BE\u09AF\
  \u09BC \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u099C\u099F\u09BF\
  \u09B2 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AA\u09CD\u09AF\u09BE\u099F\
  \u09BE\u09B0\u09CD\u09A8 \u09B8\u09BE\u09AE\u09B2\u09BE\u09A8\u09CB\u09B0 \u09A4\
  \u09BE\u09A6\u09C7\u09B0 \u09A8\u09AE\u09A8\u09C0\u09AF\u09BC\u09A4\u09BE \u098F\
  \u09AC\u0982 \u09A6\u0995\u09CD\u09B7\u09A4\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u09A4\u09BE\u09A6\u09C7\u09B0 \u09A1\u09C7\u099F\u09BE \u09AC\u09C8\u09A7\u0995\
  \u09B0\u09A3, \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u09B0\
  \u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09BE\u09B0 \u09AE\u09A4\
  \u09CB \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964."
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
weight: 11
---

## কিভাবে:
VBA এ নিয়মিত অভিব্যক্তি ব্যবহার করতে, আপনাকে প্রথমে Microsoft VBScript Regular Expressions লাইব্রেরিটি সক্রিয় করতে হবে। VBA সম্পাদকে গিয়ে `Tools` -> `References` এ যান, তারপর `Microsoft VBScript Regular Expressions 5.5` চেক করুন।

একটি স্ট্রিং এর মধ্যে একটি প্যাটার্ন আছে কি না তা খুঁজে একটি মৌলিক উদাহরণ দেখা যাক:

```vb
Sub FindPattern()
    Dim regex As Object
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = True
        .Pattern = "\bis\b"  ' শব্দ "is" খোঁজা হচ্ছে
    End With
    
    Dim testString As String
    testString = "This is a test string."
    
    If regex.Test(testString) Then
        MsgBox "Pattern found."
    Else
        MsgBox "Pattern not found."
    End If
End Sub
```

একটি স্ট্রিং এ একটি প্যাটার্ন প্রতিস্থাপন করতে:

```vb
Sub ReplacePattern()
    Dim regex As Object, replacedString As String
    Set regex = CreateObject("VBScript.RegExp")
    
    With regex
        .Global = True
        .IgnoreCase = False
        .Pattern = "\s"  ' যেকোনো সাদা স্থান চরিত্রের সাথে মিলে
    End With
    
    replacedString = regex.Replace("This is a test string.", "_")
    MsgBox replacedString  ' আউটপুট: "This_is_a_test_string."
End Sub
```

## গভীর ডুব
নিয়মিত অভিব্যক্তির প্রোগ্রামিং ভাষায় অন্তর্ভুক্তি ১৯৭০ দশকের Unix টুলস থেকে প্রায়ই ট্রেস করা হয়। VBA ভিবি স্ক্রিপ্ট নিয়মিত অভিব্যক্তি লাইব্রেরির মাধ্যমে regex একীভূত করেছে, এটি এমন কিছু অ্যাপ্লিকেশনে পাঠ্য প্রক্রিয়া কাজের গুরুত্বকে হাইলাইট করে যা সাধারণত গুরুতর পাঠ্য পরিচালনা সঙ্গে জড়িত নয় যেমন Excel বা Access।

তাদের শক্তি সত্ত্বেও, VBA এ regex মাঝে মাঝে Python বা JavaScript এর মতো আধুনিক বাস্তবায়নের তুলনায় কম অন্তর্বোধগম্য বা দক্ষ হতে পারে। উদাহরণস্বরূপ, Python-এর `re` মডিউল নামকরণ গ্রুপগুলির জন্য ব্যাপক সমর্থন এবং আরও সুবিন্যস্ত প্যাটার্ন-মেলের বৈশিষ্ট্য সরবরাহ করে, একটি পরিষ্কার এবং সম্ভবত আরও পঠনযোগ্য পদ্ধতি প্্রদান করে। তবে, VBA ইকোসিস্টেমের মধ্যে কাজ করার সময়, নিয়মিত অভিব্যক্তি Office অ্যাপ্লিকেশনে Strings নিয়ে কাজ করার সময় যে সুবিধা এবং ক্ষমতা regex নিয়ে আসে তার প্রেক্ষিতে দক্ষতা ব্যবধান প্রায়ই নগণ্য।
