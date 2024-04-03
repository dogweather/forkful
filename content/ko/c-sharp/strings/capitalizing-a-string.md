---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:35.614286-07:00
description: "\uBC29\uBC95: C#\uC740 \uB0B4\uC7A5 \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uB9CC\uB4DC\uB294 \uAC04\uB2E8\uD55C \uC811\uADFC \uBC29\uC2DD\uC744 \uC81C\
  \uACF5\uD569\uB2C8\uB2E4. \uAC00\uC7A5 \uAC04\uB2E8\uD55C \uBC29\uBC95\uC740 \uC774\
  \uB7EC\uD55C \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\
  \uC744 \uC9C1\uC811 \uC218\uC815\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uBCF4\uB2E4\
  \ \uBCF5\uC7A1\uD558\uAC70\uB098 \uD2B9\uC815\uD55C \uB300\uBB38\uC790\uD654 \uADDC\
  \uCE59(\uC608: \uAC01 \uB2E8\uC5B4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uB9CC\uB4DC\uB294 \uACBD\uC6B0)\uC5D0\u2026"
lastmod: '2024-03-13T22:44:55.209480-06:00'
model: gpt-4-0125-preview
summary: "C#\uC740 \uB0B4\uC7A5 \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C \uB9CC\
  \uB4DC\uB294 \uAC04\uB2E8\uD55C \uC811\uADFC \uBC29\uC2DD\uC744 \uC81C\uACF5\uD569\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 방법:
C#은 내장 메소드를 사용하여 문자열의 첫 글자를 대문자로 만드는 간단한 접근 방식을 제공합니다. 가장 간단한 방법은 이러한 메소드를 사용하여 문자열을 직접 수정하는 것입니다. 보다 복잡하거나 특정한 대문자화 규칙(예: 각 단어의 첫 글자를 대문자로 만드는 경우)에 대해서는 추가 라이브러리나 수동 방법이 필요할 수 있습니다. 아래 예제는 C#에서 다양한 방식으로 문자열의 첫 글자를 대문자로 만드는 방법을 보여줍니다.

### 기본 대문자화:
단어 또는 문장의 첫 글자를 대문자로 만들려면:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // 출력: "Hello world"
```

### 각 단어의 첫 글자 대문자화:
문자열의 각 단어의 첫 글자를 대문자로 만들기 위해, `System.Globalization` 네임스페이스에 있는 `TextInfo.ToTitleCase` 메소드를 사용할 수 있습니다:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // 출력: "Hello World"
```

참고: `ToTitleCase`는 나머지 글자의 대소문자를 변경하지 않고, 각 단어의 첫 글자만 대문자로 바꿉니다. 또한, 제목 규칙에 따른 특정 단어("and", "or", "of" 등)는 문화권 설정에 따라 대문자로 바뀌지 않을 수 있습니다.

### 재사용성을 위한 확장 메소드 사용:
대문자화 과정을 단순화하고 코드를 더 깨끗하고 재사용 가능하게 만들기 위해 `string` 클래스에 대한 확장 메소드를 만들 수 있습니다. 다음은 그러한 메소드를 생성하고 사용하는 방법입니다:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // 출력: "Hello world"
    }
}
```

이 확장 메소드 `Capitalize`는 네임스페이스 내의 모든 문자열 객체에서 호출할 수 있으며, C#에서 문자열 조작을 더 직관적이고 객체 지향적인 접근 방식으로 제공합니다.

### 타사 라이브러리:
C#의 표준 라이브러리는 문자열 또는 문자열 내 각 단어를 대문자로 만드는 작업에 대부분의 요구를 충족시키지만, 특정 전문 작업에는 Humanizer와 같은 타사 라이브러리가 도움이 될 수 있습니다. 그러나 문자열 또는 각 단어를 단순히 대문자로 만드는 작업의 경우, 표준 C# 메소드가 충분하고 효율적이므로 외부 의존성이 필요하지 않습니다.
