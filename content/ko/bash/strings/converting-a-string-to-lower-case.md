---
date: 2024-01-20 17:37:54.760705-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098:) \uCD08\uAE30 UNIX \uC2DC\uC2A4\
  \uD15C\uC5D0\uC11C\uB294 \uBB38\uC790\uC5F4 \uCC98\uB9AC \uAE30\uB2A5\uC774 \uAE30\
  \uBCF8\uC801\uC774\uACE0 \uC81C\uD55C\uC801\uC774\uC5C8\uC2B5\uB2C8\uB2E4. `tr`\
  \ \uBA85\uB839\uC5B4\uAC00 \uBB38\uC790\uB97C \uB300\uCCB4\uD558\uB294 \uBCF4\uD3B8\
  \uC801 \uBC29\uBC95\uC73C\uB85C \uC790\uB9AC \uC7A1\uC558\uC8E0. Bash 4.0\uBD80\uD130\
  \uB294 \uBB38\uC790\uC5F4 \uC870\uC791\uC774 \uC258 \uC790\uCCB4\uC5D0\uC11C\uB3C4\
  \ \uAC00\uB2A5\uD574\uC838 `tr`\uC744 \uC0AC\uC6A9\uD558\uC9C0 \uC54A\uACE0\uB3C4\
  \ \uC18C\uBB38\uC790 \uBCC0\uD658\uC774\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.753223-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098:) \uCD08\uAE30 UNIX \uC2DC\uC2A4\uD15C\uC5D0\
  \uC11C\uB294 \uBB38\uC790\uC5F4 \uCC98\uB9AC \uAE30\uB2A5\uC774 \uAE30\uBCF8\uC801\
  \uC774\uACE0 \uC81C\uD55C\uC801\uC774\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## How to: (어떻게 하나:)
```Bash
# 소문자로 변환하기
my_string="Hello, World!"
lowercase_string=$(echo "$my_string" | tr '[:upper:]' '[:lower:]')

echo $lowercase_string
```
출력:
```
hello, world!
```

```Bash
# Bash 4.0 이상 버전 본인 내장 기능 사용하기
my_string="Hello, World!"
lowercase_string="${my_string,,}"

echo $lowercase_string
```
출력:
```
hello, world!
```

## Deep Dive (심층 분석)
초기 UNIX 시스템에서는 문자열 처리 기능이 기본적이고 제한적이었습니다. `tr` 명령어가 문자를 대체하는 보편적 방법으로 자리 잡았죠. Bash 4.0부터는 문자열 조작이 쉘 자체에서도 가능해져 `tr`을 사용하지 않고도 소문자 변환이 가능합니다. 

`tr`은 간단하고 효율적이나 새로운 문법은 코드를 더 간결하게 만들어줍니다. 예를 들어, `${my_string,,}`는 `$my_string`의 모든 대문자를 소문자로 바꿉니다. 반대로 `${my_string^^}`은 모든 소문자를 대문자로 변환하구요. 

Bash 내장 기능의 장점은 별도의 프로세스 생성 없이 작업을 처리한다는 점입니다. `tr`은 외부 프로그램 호출을 필요로 해서 상대적으로 더 느릴 수 있습니다.

## See Also (더 보기)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [GNU 'tr' manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/index.html)
