---
aliases:
- /ko/c-sharp/removing-quotes-from-a-string/
date: 2024-01-26 03:38:47.138575-07:00
description: "C#\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uC778\uC6A9\uBD80\uD638\
  \uB97C \uC81C\uAC70\uD55C\uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB97C \uAC10\
  \uC2F8\uB294 \uADC0\uCC2E\uC740 \uC774\uC911(`\"`) \uB610\uB294 \uB2E8\uC77C(`'`)\
  \ \uC778\uC6A9\uBD80\uD638\uB97C \uC81C\uAC70\uD55C\uB2E4\uB294 \uAC83\uC744 \uC758\
  \uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\
  \uD130\uB97C \uC815\uB9AC\uD558\uAC70\uB098 \uB370\uC774\uD130\uBCA0\uC774\uC2A4\
  \ \uC785\uB825\uC744 \uC900\uBE44\uD558\uAC70\uB098 \uBB38\uC790\uC5F4\uC744 \uCD94\
  \uAC00 \uCC98\uB9AC\uC5D0 \uC548\uC804\uD558\uAC8C \uB9CC\uB4E4\uC5B4 \uC798\uBABB\
  \uB41C \uC778\uC6A9\uBD80\uD638\uAC00 \uB098\uD0C0\uB0AC\uC744 \uB54C\u2026"
lastmod: 2024-02-18 23:09:06.201050
model: gpt-4-0125-preview
summary: "C#\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uC778\uC6A9\uBD80\uD638\uB97C\
  \ \uC81C\uAC70\uD55C\uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB97C \uAC10\uC2F8\
  \uB294 \uADC0\uCC2E\uC740 \uC774\uC911(`\"`) \uB610\uB294 \uB2E8\uC77C(`'`) \uC778\
  \uC6A9\uBD80\uD638\uB97C \uC81C\uAC70\uD55C\uB2E4\uB294 \uAC83\uC744 \uC758\uBBF8\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130\
  \uB97C \uC815\uB9AC\uD558\uAC70\uB098 \uB370\uC774\uD130\uBCA0\uC774\uC2A4 \uC785\
  \uB825\uC744 \uC900\uBE44\uD558\uAC70\uB098 \uBB38\uC790\uC5F4\uC744 \uCD94\uAC00\
  \ \uCC98\uB9AC\uC5D0 \uC548\uC804\uD558\uAC8C \uB9CC\uB4E4\uC5B4 \uC798\uBABB\uB41C\
  \ \uC778\uC6A9\uBD80\uD638\uAC00 \uB098\uD0C0\uB0AC\uC744 \uB54C\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
C#에서 문자열에서 인용부호를 제거한다는 것은 텍스트를 감싸는 귀찮은 이중(`"`) 또는 단일(`'`) 인용부호를 제거한다는 것을 의미합니다. 프로그래머들은 데이터를 정리하거나 데이터베이스 입력을 준비하거나 문자열을 추가 처리에 안전하게 만들어 잘못된 인용부호가 나타났을 때 문제가 발생하지 않도록 하기 위해 이 작업을 수행합니다.

## 방법:
```csharp
string withQuotes = "\"Hello, World!\"";
Console.WriteLine($"원본: {withQuotes}");

// 이중 인용부호 제거
string withoutDoubleQuotes = withQuotes.Replace("\"", "");
Console.WriteLine($"이중 인용부호 없음: {withoutDoubleQuotes}");

// 단일 인용부호 제거 (처음부터 문자열에 포함되어 있었다고 가정)
string withSingleQuotes = "'Hello, World!'";
string withoutSingleQuotes = withSingleQuotes.Replace("'", "");
Console.WriteLine($"단일 인용부호 없음: {withoutSingleQuotes}");
```

출력값:
```
원본: "Hello, World!"
이중 인용부호 없음: Hello, World!
단일 인용부호 없음: Hello, World!
```

## 심층 분석
인용부호를 제거하는 개념은 새로운 것이나 특별히 복잡한 것은 아니지만, 인용 부호가 종종 문자열을 구분하는 데 사용되기 때문에 중요합니다. 이스케이프되지 않은 인용부호가 들어 있는 문자열이 코드 블록이나 데이터 파일에 포함되면, 문자열이 조기에 종료될 수 있어 오류 또는 보안 문제(예: 인젝션 공격)를 일으킬 수 있습니다.

역사적으로, 인용부호 처리는 데이터 처리에서 유효성 검사 및 살균 과정의 일부였습니다. `.Replace()` 메소드는 단순 문자열에서 인용부호를 제거하는 것이 간단하지만, 중첩된 인용부호나 조건부 제거와 같이 더 복잡한 상황을 다루는 경우 정규 표현식과 같은 더 고급 기술이 필요할 수 있습니다.

`.Replace()`에 대한 대안으로는 고정된 문자가 아닌 패턴을 다룰 때나 세밀한 제어가 필요할 때 `Regex` 클래스의 메소드를 사용할 수 있습니다. 예를 들어, 이스케이프된 문자를 다룰 때 `Regex.Unescape()`가 유용할 수 있습니다.

구현 측면에서, C#의 문자열은 변경 불가능(immutable)하므로 `.Replace()`를 사용할 때마다 새로운 문자열이 생성됩니다. 이것은 작은 또는 일회성 작업에 대해서는 큰 문제가 아니지만, 대량 또는 다수의 문자열에 대해 성능 측면에서 염두에 두어야 할 사항입니다.

## 참고자료:
- [String.Replace 메소드 문서화](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.replace?view=netframework-4.8)
- [.NET의 정규 표현식](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expressions)
- [안전한 문자열 처리 모범 사례](https://www.owasp.org/index.php/Data_Validation)
