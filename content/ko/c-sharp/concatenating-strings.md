---
title:    "C#: 문자열 연결하기"
keywords: ["C#"]
---

{{< edit_this_page >}}

## 왜?

문자열 연결(concatenation)은 프로그래밍에서 매우 중요한 개념입니다. 다른 문자열들을 하나로 합치는 것은 정보를보다 쉽고 간편하게 표현할 수 있도록 해줍니다.

## 방법

아래와 같이 사용할 수 있습니다:

```C#
string firstName = "John";
string lastName = "Doe";
string fullName = firstName + " " + lastName;

Console.WriteLine(fullName);

// Output: John Doe
```

문자열을 합치기위해 `+` 연산자를 사용하는 것이 매우 간단하고 직관적입니다. 또는 `string.Format()` 함수를 사용해도 동일한 결과를 얻을 수 있습니다.

```C#
string firstName = "John";
string lastName = "Doe";
string fullName = string.Format("{0} {1}", firstName, lastName);

Console.WriteLine(fullName);

// Output: John Doe
```

## 깊이 파고들기

문자열 연결의 더 깊은 개념을 알고 싶다면, `StringBuilder` 클래스를 사용하는 것이 좋습니다. `StringBuilder` 클래스는 문자열을 합치는 과정에서 메모리를 더 효율적으로 사용할 수 있도록 해줍니다.

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Hello").Append(" ").Append("World");

Console.WriteLine(sb.ToString());

// Output: Hello World
```

그러나 문장이 복잡해지고, 문자열이 많아질 경우에만 `StringBuilder` 클래스를 사용하는 것이 좋습니다. 간단한 문자열을 합치는 경우에는 `+` 연산자를 사용하는 것이 더 효율적일 수 있습니다.

## 또 다른 방법들을 알고 싶다면 아래 링크들을 확인하세요:
- [String.Concat Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat?view=netframework-4.8)
- [StringBuilder Class (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=netframework-4.8)
- [How StringBuilder Works (C# Corner)](https://www.c-sharpcorner.com/UploadFile/4d9083/how-stringbuilder-works-behind-the-scene-in-C-Sharp/)