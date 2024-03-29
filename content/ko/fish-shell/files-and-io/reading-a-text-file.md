---
date: 2024-01-20 17:54:40.895563-07:00
description: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294 \uAC83\uC740 \uD30C\
  \uC77C\uC758 \uB0B4\uC6A9\uC744 \uBD88\uB7EC\uC640 \uBCF4\uAC70\uB098 \uCC98\uB9AC\
  \uD558\uAE30 \uC704\uD568\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294\
  \ \uB370\uC774\uD130\uB97C \uC0AC\uC6A9\uD558\uAC70\uB098 \uD504\uB85C\uADF8\uB7A8\
  \ \uAC04\uC5D0 \uC815\uBCF4\uB97C \uC804\uB2EC\uD558\uAE30 \uC704\uD574 \uD30C\uC77C\
  \uC744 \uC77D\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.880786-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\uB294 \uAC83\uC740 \uD30C\uC77C\
  \uC758 \uB0B4\uC6A9\uC744 \uBD88\uB7EC\uC640 \uBCF4\uAC70\uB098 \uCC98\uB9AC\uD558\
  \uAE30 \uC704\uD568\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB370\
  \uC774\uD130\uB97C \uC0AC\uC6A9\uD558\uAC70\uB098 \uD504\uB85C\uADF8\uB7A8 \uAC04\
  \uC5D0 \uC815\uBCF4\uB97C \uC804\uB2EC\uD558\uAE30 \uC704\uD574 \uD30C\uC77C\uC744\
  \ \uC77D\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일을 읽는 것은 파일의 내용을 불러와 보거나 처리하기 위함입니다. 프로그래머는 데이터를 사용하거나 프로그램 간에 정보를 전달하기 위해 파일을 읽습니다.

## How to (방법):
Fish Shell에서 텍스트 파일을 읽으세요.

```Fish Shell
cat example.txt # 파일 전체 내용을 출력한다

# 특정 행만 표시하고 싶다면
sed -n '5p' example.txt # 5번째 줄만 출력

# 행 범위를 지정하여 출력
sed -n '2,4p' example.txt # 2부터 4번째 줄까지 출력
```
Sample Output:
```
이것은 example.txt의 내용입니다.
```

## Deep Dive (심층 분석):
도스(DOS) 시절부터 텍스트 파일 읽기는 기본적인 컴퓨터 작업이었습니다. 유닉스/리눅스 커맨드라인은 'cat', 'more', 'less', 'head', 'tail' 같은 도구들로 텍스트 파일을 읽을 수 있게 해줍니다. Fish Shell은 이런 도구들을 사용하기 쉽게 만듭니다. 예를 들어, `read` 명령어는 텍스트에서 변수를 읽는데 사용되고, `string` 함수로 파일 내용을 분석할 수 있습니다. 대안으로 파이썬이나 루비 같은 스크립트 언어를 사용할 수도 있습니다.

## See Also (관련 자료):
- Fish Shell 공식 문서: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- 유닉스 텍스트 처리를 위한 안내서: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
