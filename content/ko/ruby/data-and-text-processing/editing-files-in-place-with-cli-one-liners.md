---
date: 2024-01-27 16:21:02.007821-07:00
description: "CLI(Command Line Interface) \uD55C \uC904 \uBA85\uB839\uC744 \uC0AC\uC6A9\
  \uD55C Ruby\uC758 \uD30C\uC77C \uD604\uC7A5 \uD3B8\uC9D1\uC740 \uC5D0\uB514\uD130\
  \uC5D0\uC11C \uC5F4\uC5B4 \uBCC0\uACBD \uD6C4 \uB2E4\uC2DC \uC800\uC7A5\uD560 \uD544\
  \uC694 \uC5C6\uC774 \uD130\uBBF8\uB110\uC5D0\uC11C \uC9C1\uC811 \uD30C\uC77C\uC744\
  \ \uC218\uC815\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uC774 \uAE30\
  \uC220\uC740 \uBE60\uB978 \uC218\uC815, \uC77C\uAD04 \uC5C5\uB370\uC774\uD2B8 \uB610\
  \uB294 \uBC18\uBCF5 \uC791\uC5C5 \uC790\uB3D9\uD654\uC5D0 \uB9E4\uC6B0 \uC720\uC6A9\
  \uD558\uC5EC\u2026"
lastmod: '2024-03-13T22:44:55.988977-06:00'
model: gpt-4-0125-preview
summary: "CLI(Command Line Interface) \uD55C \uC904 \uBA85\uB839\uC744 \uC0AC\uC6A9\
  \uD55C Ruby\uC758 \uD30C\uC77C \uD604\uC7A5 \uD3B8\uC9D1\uC740 \uC5D0\uB514\uD130\
  \uC5D0\uC11C \uC5F4\uC5B4 \uBCC0\uACBD \uD6C4 \uB2E4\uC2DC \uC800\uC7A5\uD560 \uD544\
  \uC694 \uC5C6\uC774 \uD130\uBBF8\uB110\uC5D0\uC11C \uC9C1\uC811 \uD30C\uC77C\uC744\
  \ \uC218\uC815\uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4."
title: "CLI \uD55C \uC904 \uBA85\uB839\uC5B4\uB85C \uD30C\uC77C\uC744 \uC81C\uC790\
  \uB9AC\uC5D0\uC11C \uD3B8\uC9D1\uD558\uAE30"
weight: 32
---

## 무엇 & 왜?

CLI(Command Line Interface) 한 줄 명령을 사용한 Ruby의 파일 현장 편집은 에디터에서 열어 변경 후 다시 저장할 필요 없이 터미널에서 직접 파일을 수정할 수 있게 해줍니다. 이 기술은 빠른 수정, 일괄 업데이트 또는 반복 작업 자동화에 매우 유용하여 시간과 노력을 모두 절약할 수 있습니다.

## 방법:

Ruby는 명령 줄에서 바로 파일을 현장에서 편집하는 간단한 방법을 제공합니다. Ruby의 `-i` 스위치를 사용하면 제공된 파일에 직접 작업하도록 Ruby에 지시할 수 있습니다. 실제로 이것이 어떻게 작동하는지 몇 가지 예제를 통해 살펴보겠습니다. 다음과 같은 내용이 있는 `greetings.txt` 파일이 있다고 가정해 봅시다.

```
Hello, world!
Hello, Ruby!
Hello, programming!
```

그리고 단어 "Hello"를 "Hi"로 바꾸고 싶다면 다음과 같이 할 수 있습니다.

```Ruby
ruby -i -pe "gsub(/Hello/, 'Hi')" greetings.txt
```

이 명령을 실행한 후 `greetings.txt`는 다음과 같이 업데이트됩니다.

```
Hi, world!
Hi, Ruby!
Hi, programming!
```

데이터를 망칠까 걱정된다면, Ruby가 당신을 보호합니다. `-i` 스위치에 확장자를 제공함으로써, Ruby는 변경을 실행하기 전에 백업을 생성합니다. 예를 들어:

```Ruby
ruby -i.bak -pe "gsub(/Hello/, 'Bye')" greetings.txt
```

이제 편집된 `greetings.txt`와 함께 동일한 디렉토리에 원본 내용을 담고 있는 `greetings.txt.bak`을 찾을 수 있습니다.

## 심층 분석

Ruby에서 현장 파일 편집의 마법은 Perl과 같은 텍스트 처리 기능과 Ruby 자체의 문법적 우아함을 결합한 데서 비롯됩니다. 역사적으로, Perl은 특히 텍스트 조작을 위한 빠른 한 줄 스크립팅에 가장 적합한 언어였습니다. Ruby는 이 패러다임을 채택하여 강력한 명령 줄 스크립팅 기능을 가능하게 했습니다.

현장 편집을 위한 다른 언어의 대안으로는 원래의 Perl 자체와 Unix 시스템에서 스트림 에디터인 sed가 있습니다. 각각은 자신의 강점이 있습니다 — Perl은 텍스트 처리 기능으로 알려져 있으며 sed는 스트림 편집 작업을 위한 단순성에서 누구도 따라올 수 없습니다. 그러나 Ruby는 특히 이미 Ruby에 익숙한 사람들에게 더 읽기 쉽고 사용자 친화적인 문법으로 강력한 텍스트 조작을 제공하면서 균형을 제공합니다.

구현 측면에서, Ruby의 현장 편집은 원래 파일의 이름을 변경하고, 원래 파일 이름으로 새 파일을 생성한 다음, 이 새 파일에 변경 사항을 쓰면서 이름이 변경된 원본에서 읽게 됩니다. 이 접근 방식은 작업의 원자성을 보장하여 전체 파일이 성공적으로 처리되거나 변경 사항이 전혀 이루어지지 않아 편집 과정 중 데이터의 무결성을 보호합니다. 이 메커니즘은 또한 전원 고장 또는 프로세스 종료와 같은 중단에 대한 회복력을 제공하며, 적어도 백업이 온전히 남아 있음을 보장합니다.

요약하자면, Ruby의 현장 파일 편집은 명령 줄에서 직접 텍스트 조작 작업을 위한 강력함, 단순함, 우아함의 혼합을 제공하는 스크립팅 언어로서의 유용성을 증명합니다.
