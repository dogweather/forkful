---
title:                "ইয়ামেল নিয়ে কাজ করা"
date:                  2024-03-17T18:37:59.889507-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?
YAML, যার অর্থ YAML Ain't Markup Language, সকল প্রোগ্রামিং ভাষার জন্য একটি মানব-বান্ধব ডাটা সেরিয়ালাইজেশন মান। প্রোগ্রামাররা এটি কনফিগারেশন ফাইল, ইন্টার-প্রসেস মেসেজিং, এবং ডাটা স্টোরেজের জন্য ব্যবহার করে কারণ এর পঠনীয়তা XML অথবা JSON এর মত অন্যান্য ডাটা ফরম্যাটের তুলনায় অনেক বেশি সাধারণ ইংরেজির কাছাকাছি, যা এটি বুঝতে এবং লিখতে সহজ করে তোলে।

## কিভাবে:
Swift এ YAML পার্সিং এবং সিরিয়ালাইজেশনের জন্য অন্তর্নির্মিত সমর্থন অন্তর্ভুক্ত নেই, যা তৃতীয়-পক্ষের লাইব্রেরীগুলিকে ব্যবহারের প্রয়োজন করে তোলে। একটি জনপ্রিয় চয়েস হল `Yams`, Swift এ YAML এর সঙ্গে কাজ করার জন্য একটি লাইব্রেরী।

প্রথমে, আপনাকে আপনার প্রজেক্টে `Yams` যোগ করতে হবে। আপনি যদি Swift Package Manager ব্যবহার করেন, তাহলে `Package.swift` ফাইলে এটি একটি নির্ভরশীলতা হিসাবে যোগ করতে পারেন:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### Swift এ YAML পার্স করা
ধরা যাক, আপনার একটি সাধারণ অ্যাপের জন্য নিচের মতো YAML কনফিগারেশন রয়েছে:

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

`Yams` ব্যবহার করে Swift এ এই YAML স্ট্রিং পার্স করার উপায় নিম্নরূপ:

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // পার্স করা ডাটা অ্যাক্সেস করার উদাহরণ
        if let name = data["name"] as? String {
            print("App Name: \(name)")
        }
    }
} catch {
    print("YAML পার্স করার সময় ত্রুটি: \(error)")
}
```

উদাহরণ আউটপুট:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
App Name: MyApp
```

### Swift অবজেক্টগুলিকে YAML এ সিরিয়ালাইজ করা
`Yams` দিয়ে একটি Swift অবজেক্টকে আবার YAML স্ট্রিং এ রূপান্তর করা সহজসাধ্য। ধরুন, আপনার সিরিয়ালাইজ করার জন্য একই ডাটা স্ট্রাকচার প্রয়োজন:

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("YAML এ সিরিয়ালাইজ করার সময় ত্রুটি: \(error)")
}
```

এটি একটি YAML-ফর্ম্যাটে স্ট্রিং উৎপাদন করবে:

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

এই উদাহরণগুলি Swift অ্যাপ্লিকেশনগুলিতে YAML এর সাথে কাজ করার জন্য মৌলিক অপারেশন প্রদর্শন করে। মনে রাখবেন, YAML মানব পঠনীয়তা এবং ব্যবহারের সহজতার ক্ষেত্রে অতুলনীয় হলেও, আপনার অ্যাপ্লিকেশনের বিশেষ চাহিদা, বিশেষ করে পারফরমেন্স এবং জটিলতার সম্পর্কে সবসময় বিবেচনা করুন যখন আপনি আপনার ডাটা সিরিয়ালাইজেশন ফরম্যাট নির্বাচন করছেন।
