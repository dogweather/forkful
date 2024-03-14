---
date: 2024-01-26 04:12:29.225392-07:00
description: "REPL\uC774\uB780 Read-Eval-Print Loop\uC758 \uC57D\uC790\uB85C, C# \uCF54\
  \uB4DC\uB97C \uC785\uB825\uD558\uC5EC \uC0C1\uD638\uC791\uC6A9\uD558\uBA70 \uC2E4\
  \uD589\uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uBCF5\uC7A1\uD55C \uD504\uB85C\uC81D\uD2B8 \uAD6C\uC131 \uC5C6\uC774\
  \ C#\uB97C \uBE60\uB974\uAC8C \uC2E4\uD5D8, \uB514\uBC84\uAE45\uD558\uAC70\uB098\
  \ \uD559\uC2B5\uD558\uB294 \uB370\uC5D0 REPL\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:55.236118-06:00'
model: gpt-4-0125-preview
summary: "REPL\uC774\uB780 Read-Eval-Print Loop\uC758 \uC57D\uC790\uB85C, C# \uCF54\
  \uB4DC\uB97C \uC785\uB825\uD558\uC5EC \uC0C1\uD638\uC791\uC6A9\uD558\uBA70 \uC2E4\
  \uD589\uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uBCF5\uC7A1\uD55C \uD504\uB85C\uC81D\uD2B8 \uAD6C\uC131 \uC5C6\uC774\
  \ C#\uB97C \uBE60\uB974\uAC8C \uC2E4\uD5D8, \uB514\uBC84\uAE45\uD558\uAC70\uB098\
  \ \uD559\uC2B5\uD558\uB294 \uB370\uC5D0 REPL\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
REPL이란 Read-Eval-Print Loop의 약자로, C# 코드를 입력하여 상호작용하며 실행할 수 있게 합니다. 프로그래머들은 복잡한 프로젝트 구성 없이 C#를 빠르게 실험, 디버깅하거나 학습하는 데에 REPL을 사용합니다.

## 사용 방법:
C# 환경에서 C# Interactive 창을 사용하거나 터미널에서 `dotnet-script`를 실행하여 REPL을 시작하세요. 사용 예시는 다음과 같습니다:

```csharp
> var greeting = "안녕, REPL!";
> Console.WriteLine(greeting);
안녕, REPL!
> 
```

즉각적인 피드백을 받을 수 있습니다. 컴파일하고 실행할 필요 없이 코드를 작성하고 결과를 확인하세요.

## 심층 탐구
REPL은 Lisp에서 현대 언어로 이어져, 파이콘과 같은 동적 언어에서 번성했습니다. C#에 대해서는, Roslyn이 개발자에게 REPL을 더 가까이 가져왔습니다. Roslyn을 위한 `csi`와 .NET Core를 위한 `dotnet-script`는 탄탄한 옵션입니다. 더 깊은 부분: 이들은 코드를 한 번에 모두가 아닌 한 줄씩 평가합니다, 이는 전형적인 C# 애플리케이션과는 다른 실행 모델을 의미합니다. 이것은 실행 간의 상태 유지와 변수의 범위에 영향을 미칩니다.

Visual Studio의 C# Interactive 창은 Roslyn에 의해 구동되는 REPL입니다. Intellisense, 다중 참조 및 NuGet 패키지 지원이 있습니다. 초기 커맨드라인 실험에서 한걸음 나아간 것입니다.

대체 언어의 경우, Python은 `IDLE`을 사용하고, JavaScript는 Node.js의 REPL을 가지며, F#은 `F# Interactive`와 함께 제공됩니다. 각각은 소규모 코드 스니펫을 테스트하거나 언어 기능을 이해하는 데 매우 유용한 즉각적인 피드백 루프를 제공합니다.

## 참고
- [.NET Core `dotnet-script` REPL](https://github.com/filipw/dotnet-script)
