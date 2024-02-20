---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:39.647709-07:00
description: "C#\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uBB38\uC790\
  \uC5F4 \uB0B4\uC5D0\uC11C \uD328\uD134 \uB9E4\uCE6D\uC744 \uC704\uD55C \uAC15\uB825\
  \uD55C \uB3C4\uAD6C\uB85C, \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uB370\uC774\uD130\
  \uB97C \uD6A8\uC728\uC801\uC73C\uB85C \uAC80\uC0C9, \uB300\uCCB4, \uBD84\uD560 \uB610\
  \uB294 \uCD94\uCD9C\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB294 \uC774\uBA54\uC77C \uD615\uC2DD \uAC80\uC0AC\uC640 \uAC19\
  \uC740 \uAC04\uB2E8\uD55C \uAC80\uC99D \uC791\uC5C5\uBD80\uD130 \uBCF5\uC7A1\uD55C\
  \ \uD14D\uC2A4\uD2B8 \uCC98\uB9AC \uC791\uC5C5\uC5D0 \uC774\uB974\uAE30\uAE4C\uC9C0\
  , \uADF8\u2026"
lastmod: 2024-02-19 22:05:14.130751
model: gpt-4-0125-preview
summary: "C#\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uBB38\uC790\
  \uC5F4 \uB0B4\uC5D0\uC11C \uD328\uD134 \uB9E4\uCE6D\uC744 \uC704\uD55C \uAC15\uB825\
  \uD55C \uB3C4\uAD6C\uB85C, \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uB370\uC774\uD130\
  \uB97C \uD6A8\uC728\uC801\uC73C\uB85C \uAC80\uC0C9, \uB300\uCCB4, \uBD84\uD560 \uB610\
  \uB294 \uCD94\uCD9C\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB294 \uC774\uBA54\uC77C \uD615\uC2DD \uAC80\uC0AC\uC640 \uAC19\
  \uC740 \uAC04\uB2E8\uD55C \uAC80\uC99D \uC791\uC5C5\uBD80\uD130 \uBCF5\uC7A1\uD55C\
  \ \uD14D\uC2A4\uD2B8 \uCC98\uB9AC \uC791\uC5C5\uC5D0 \uC774\uB974\uAE30\uAE4C\uC9C0\
  , \uADF8\u2026"
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이고 왜 쓰나요?
C#에서 정규 표현식(regex)은 문자열 내에서 패턴 매칭을 위한 강력한 도구로, 프로그래머가 데이터를 효율적으로 검색, 대체, 분할 또는 추출할 수 있게 해줍니다. 프로그래머는 이메일 형식 검사와 같은 간단한 검증 작업부터 복잡한 텍스트 처리 작업에 이르기까지, 그 유연성과 성능 때문에 정규 표현식을 사용합니다.

## 어떻게 사용하나요?

### 간단한 패턴 매칭
특정 패턴이 문자열에 포함되어 있는지 확인하려면 `System.Text.RegularExpressions` 네임스페이스에서 `Regex.IsMatch` 메서드를 사용할 수 있습니다.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // 출력: True
    }
}
```

### 데이터 추출
정규 표현식에서 그룹을 사용하여 문자열로부터 데이터를 추출하는 것은 `Regex.Match` 메서드로 할 수 있습니다.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Date: 2023-04-12";
        string pattern = @"Date: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Year: {match.Groups[1].Value}");  // 출력: Year: 2023
            Console.WriteLine($"Month: {match.Groups[2].Value}");  // 출력: Month: 04
            Console.WriteLine($"Day: {match.Groups[3].Value}");  // 출력: Day: 12
        }
    }
}
```

### 텍스트 대체
`Regex.Replace` 메서드를 사용하면 지정된 패턴과 일치하는 문자열 내의 텍스트를 대체할 수 있습니다.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Visit Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // 출력: Visit Google!
    }
}
```

### 문자열 분할
`Regex.Split` 메서드를 사용하면 정규 표현식 패턴에 기반해 문자열을 배열로 분할할 수 있습니다.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "one,two,three,four,five";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // 출력: 
        // one
        // two
        // three
        // four
        // five
    }
}
```

### 서드 파티 라이브러리 사용하기
.NET Framework는 정규 표현식에 대한 광범위한 지원을 제공하지만, C#에서 Perl 호환 정규 표현식(PCRE)를 제공하는 `PCRE.NET`과 같은 서드 파티 라이브러리도 있습니다. .NET의 구현에서 사용할 수 없는 Perl의 정규 표현식 엔진의 기능이나 문법이 필요한 경우 유용할 수 있습니다.

`PCRE.NET`을 사용하려면 먼저 NuGet 패키지를 설치해야 하며, 그 후에는 기본 .NET 정규 표현식 클래스를 사용하는 것과 유사하게 사용할 수 있습니다.

```csharp
// PCRE.NET 사용 예제
// 참고: PCRE.NET의 고유 기능을 보여주는 위의 예제들과 유사한 샘플을 상상해 보세요.
```

정규 표현식을 위해 서드 파티 라이브러리를 통합할 때 항상 그들의 문서를 상세히 확인하여 사용법과 호환성 정보를 참조하십시오.
