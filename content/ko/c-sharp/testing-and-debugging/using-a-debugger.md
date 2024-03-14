---
date: 2024-01-26 03:48:41.744668-07:00
description: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uC774\uB780 \uCF54\uB4DC\uB97C \uD14C\
  \uC2A4\uD2B8\uD558\uACE0 \uC9C4\uB2E8\uD558\uAE30 \uC704\uD574 \uC804\uBB38 \uB3C4\
  \uAD6C\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBC84\uADF8\uB97C \uC81C\uAC70\uD558\
  \uACE0, \uCF54\uB4DC \uD750\uB984\uC744 \uC774\uD574\uD558\uBA70, \uCF54\uB4DC\uAC00\
  \ \uC608\uC0C1\uB300\uB85C \uB3D9\uC791\uD558\uB294\uC9C0 \uD655\uC778\uD558\uAE30\
  \ \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4. \uC774\uB294 \uCF54\uB4DC\
  \uC758 \uB1CC\uC5D0 \uB300\uD55C \uD604\uBBF8\uACBD\uC744 \uAC00\uC9C0\uB294 \uAC83\
  \uACFC \uAC19\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.240710-06:00'
model: gpt-4-0125-preview
summary: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uC774\uB780 \uCF54\uB4DC\uB97C \uD14C\uC2A4\
  \uD2B8\uD558\uACE0 \uC9C4\uB2E8\uD558\uAE30 \uC704\uD574 \uC804\uBB38 \uB3C4\uAD6C\
  \uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBC84\uADF8\uB97C \uC81C\uAC70\uD558\uACE0\
  , \uCF54\uB4DC \uD750\uB984\uC744 \uC774\uD574\uD558\uBA70, \uCF54\uB4DC\uAC00 \uC608\
  \uC0C1\uB300\uB85C \uB3D9\uC791\uD558\uB294\uC9C0 \uD655\uC778\uD558\uAE30 \uC704\
  \uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4. \uC774\uB294 \uCF54\uB4DC\uC758\
  \ \uB1CC\uC5D0 \uB300\uD55C \uD604\uBBF8\uACBD\uC744 \uAC00\uC9C0\uB294 \uAC83\uACFC\
  \ \uAC19\uC2B5\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거 사용이란 코드를 테스트하고 진단하기 위해 전문 도구를 사용하는 것을 의미합니다. 프로그래머들은 버그를 제거하고, 코드 흐름을 이해하며, 코드가 예상대로 동작하는지 확인하기 위해 이를 수행합니다. 이는 코드의 뇌에 대한 현미경을 가지는 것과 같습니다.

## 사용 방법:
작은 프로그램이 제대로 동작하지 않는다고 상상해보세요:

```C#
static void Main()
{
    int result = Sum(1, 2);
    Console.WriteLine(result);
}

static int Sum(int a, int b)
{
    return a + a; // 오류, a + b가 되어야 함
}
```

Visual Studio의 디버거를 사용하여, `return a + a;` 옆에 있는 왼쪽 여백을 클릭하여 중단점을 설정하세요. 프로그램을 실행할 때(F5), 실행이 그곳에서 멈출 것입니다. 변수 위에 마우스를 올려 그 값들을 검사하거나, 즉시 실행 창을 사용하여 표현식을 평가해보세요. `a`는 1이고 `b`는 2이지만, `a + a`는 우리가 예상한 합이 아닙니다. 이를 `a + b`로 변경하고 계속 실행(F5)하면, 콘솔이 3을 출력할 것입니다.

## 심층 분석
디버깅의 역사는 1940년대로 거슬러 올라가며, 초기 컴퓨터에서 진짜 벌레(나방)이 발견됐을 때부터 시작됩니다. 오늘날의 디버거는 Visual Studio에 있는 것처럼 중단점, 단계별 실행, 감시 창 등과 같은 강력한 기능을 제공합니다.

Visual Studio의 디버거 대안에는 C 스타일 언어용 GDB나 Python용 pdb와 같은 오픈 소스 옵션과 C# 및 기타 언어에 대한 디버깅 도구를 제공하는 JetBrains Rider나 VS Code와 같은 크로스 플랫폼 IDE가 포함됩니다.

디버거 구현에 뛰어들 때, 당신은 애플리케이션의 프로세스에 연결하는 프로그램을 보게 됩니다. 이는 기계어를 해석하고, 메모리 상태를 관리하며, 실행 흐름을 제어합니다. 이런 무거운 작업은 효율적인 디버깅을 위해 필수적인데, 이것이 바로 디버그 모드가 이러한 후크가 존재하지 않는 릴리스 모드보다 느리게 실행되는 이유입니다.

## 참고 자료
- [Visual Studio 디버거 문서](https://docs.microsoft.com/ko-kr/visualstudio/debugger/)
- [디버깅 전략](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
