---
date: 2024-01-26 04:15:34.843658-07:00
description: "REPL(Read-Eval-Print Loop)\uC740 \uC0AC\uC6A9\uC790\uC758 \uB2E8\uC77C\
  \ \uC785\uB825\uC744 \uCC98\uB9AC\uD558\uACE0, \uCF54\uB4DC\uB97C \uC2E4\uD589\uD558\
  \uBA70, \uACB0\uACFC\uB97C \uBC18\uD658\uD558\uB294 \uC778\uD130\uB799\uD2F0\uBE0C\
  \ \uC258\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC989\uAC01\
  \uC801\uC778 \uD53C\uB4DC\uBC31\uACFC \uBC18\uBCF5\uC744 \uAC00\uB2A5\uD558\uAC8C\
  \ \uD568\uC73C\uB85C\uC368, \uBE60\uB978 \uC2E4\uD5D8, \uB514\uBC84\uAE45 \uB610\
  \uB294 \uD559\uC2B5\uC744 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
lastmod: '2024-03-11T00:14:28.970070-06:00'
model: gpt-4-0125-preview
summary: "REPL(Read-Eval-Print Loop)\uC740 \uC0AC\uC6A9\uC790\uC758 \uB2E8\uC77C \uC785\
  \uB825\uC744 \uCC98\uB9AC\uD558\uACE0, \uCF54\uB4DC\uB97C \uC2E4\uD589\uD558\uBA70\
  , \uACB0\uACFC\uB97C \uBC18\uD658\uD558\uB294 \uC778\uD130\uB799\uD2F0\uBE0C \uC258\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC989\uAC01\uC801\
  \uC778 \uD53C\uB4DC\uBC31\uACFC \uBC18\uBCF5\uC744 \uAC00\uB2A5\uD558\uAC8C \uD568\
  \uC73C\uB85C\uC368, \uBE60\uB978 \uC2E4\uD5D8, \uB514\uBC84\uAE45 \uB610\uB294 \uD559\
  \uC2B5\uC744 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178 (REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?
REPL(Read-Eval-Print Loop)은 사용자의 단일 입력을 처리하고, 코드를 실행하며, 결과를 반환하는 인터랙티브 쉘입니다. 프로그래머들은 즉각적인 피드백과 반복을 가능하게 함으로써, 빠른 실험, 디버깅 또는 학습을 위해 이를 사용합니다.

## 어떻게 사용하는가:
Java 9에서 도입된 `jshell` 도구를 사용하여 Java에서 REPL을 시작하는 것은 간단합니다. 기본 세션을 시작하는 방법은 다음과 같습니다:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  생성된 메소드 sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

언제든지 `/exit`로 종료할 수 있습니다.

```Java
jshell> /exit
|  안녕히 가세요
```

## 심층 탐구
`jshell` 이전에는 Java 프로그래머들은 Python이나 Ruby 개발자들과 달리 공식 REPL이 없었습니다. 그들은 IDE를 사용하거나 사소한 작업조차 전체 프로그램을 작성해야 했습니다. Java 9부터 `jshell`은 그 격차를 메웠습니다.

대안으로는 온라인 컴파일러나 IDE 플러그인이 있지만, `jshell`의 즉각성과는 일치하지 않습니다. 내부적으로, `jshell`은 코드 조각을 실행하기 위해 Java Compiler API를 사용하는데, 이는 꽤 멋집니다. 이는 단순한 놀이터 이상입니다 - 라이브러리를 가져오고, 클래스를 정의하는 등 더 많은 것을 할 수 있습니다. 이로 인해 프로토타이핑을 위한 강력한 도구가 됩니다.

## 참고 자료
- [JShell 사용자 가이드](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java Platform, Standard Edition 도구 참조](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java Compiler API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
