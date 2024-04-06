---
date: 2024-01-26 03:48:41.744668-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: \uB514\uBC84\uAE45\uC758 \uC5ED\uC0AC\uB294\
  \ 1940\uB144\uB300\uB85C \uAC70\uC2AC\uB7EC \uC62C\uB77C\uAC00\uBA70, \uCD08\uAE30\
  \ \uCEF4\uD4E8\uD130\uC5D0\uC11C \uC9C4\uC9DC \uBC8C\uB808(\uB098\uBC29)\uC774 \uBC1C\
  \uACAC\uB410\uC744 \uB54C\uBD80\uD130 \uC2DC\uC791\uB429\uB2C8\uB2E4. \uC624\uB298\
  \uB0A0\uC758 \uB514\uBC84\uAC70\uB294 Visual Studio\uC5D0 \uC788\uB294 \uAC83\uCC98\
  \uB7FC \uC911\uB2E8\uC810, \uB2E8\uACC4\uBCC4 \uC2E4\uD589, \uAC10\uC2DC \uCC3D\
  \ \uB4F1\uACFC \uAC19\uC740 \uAC15\uB825\uD55C \uAE30\uB2A5\uC744 \uC81C\uACF5\uD569\
  \uB2C8\uB2E4. Visual\u2026"
lastmod: '2024-04-05T22:51:09.579254-06:00'
model: gpt-4-0125-preview
summary: "\uB514\uBC84\uAE45\uC758 \uC5ED\uC0AC\uB294 1940\uB144\uB300\uB85C \uAC70\
  \uC2AC\uB7EC \uC62C\uB77C\uAC00\uBA70, \uCD08\uAE30 \uCEF4\uD4E8\uD130\uC5D0\uC11C\
  \ \uC9C4\uC9DC \uBC8C\uB808(\uB098\uBC29)\uC774 \uBC1C\uACAC\uB410\uC744 \uB54C\uBD80\
  \uD130 \uC2DC\uC791\uB429\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
weight: 35
---

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
