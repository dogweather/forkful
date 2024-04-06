---
date: 2024-01-20 17:54:40.895563-07:00
description: "How to (\uBC29\uBC95): \uB3C4\uC2A4(DOS) \uC2DC\uC808\uBD80\uD130 \uD14D\
  \uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30\uB294 \uAE30\uBCF8\uC801\uC778 \uCEF4\uD4E8\
  \uD130 \uC791\uC5C5\uC774\uC5C8\uC2B5\uB2C8\uB2E4. \uC720\uB2C9\uC2A4/\uB9AC\uB205\
  \uC2A4 \uCEE4\uB9E8\uB4DC\uB77C\uC778\uC740 'cat', 'more', 'less', 'head', 'tail'\
  \ \uAC19\uC740 \uB3C4\uAD6C\uB4E4\uB85C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC77D\
  \uC744 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. Fish Shell\uC740 \uC774\uB7F0\
  \u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:10.082274-06:00'
model: gpt-4-1106-preview
summary: "\uB3C4\uC2A4(DOS) \uC2DC\uC808\uBD80\uD130 \uD14D\uC2A4\uD2B8 \uD30C\uC77C\
  \ \uC77D\uAE30\uB294 \uAE30\uBCF8\uC801\uC778 \uCEF4\uD4E8\uD130 \uC791\uC5C5\uC774\
  \uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

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
