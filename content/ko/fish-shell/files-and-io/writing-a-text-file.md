---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:51.915954-07:00
description: "\uBC29\uBC95: Fish\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC5D0\
  \ \uC791\uC131\uD558\uB824\uBA74, `echo` \uBA85\uB839\uACFC \uB9AC\uB2E4\uC774\uB809\
  \uC158 \uC5F0\uC0B0\uC790\uB97C \uD568\uAED8 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4. \uD30C\uC77C \uC4F0\uAE30\uB97C \uC704\uD55C \uC778\uAE30 \uC788\uB294\
  \ \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB294 Fish\uC5D0 \uD2B9\uBCC4\uD788\
  \ \uC5C6\uC9C0\uB9CC, \uC258\uC758 \uB0B4\uC7A5 \uBA85\uB839\uC740 \uC774 \uBAA9\
  \uC801\uC5D0 \uB300\uD574 \uC9C1\uAD00\uC801\uC774\uACE0 \uD6A8\uC728\uC801\uC785\
  \uB2C8\uB2E4. #."
lastmod: '2024-03-13T22:44:55.882227-06:00'
model: gpt-4-0125-preview
summary: "Fish\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC5D0 \uC791\uC131\uD558\
  \uB824\uBA74, `echo` \uBA85\uB839\uACFC \uB9AC\uB2E4\uC774\uB809\uC158 \uC5F0\uC0B0\
  \uC790\uB97C \uD568\uAED8 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:
Fish에서 텍스트 파일에 작성하려면, `echo` 명령과 리다이렉션 연산자를 함께 사용할 수 있습니다. 파일 쓰기를 위한 인기 있는 제3자 라이브러리는 Fish에 특별히 없지만, 쉘의 내장 명령은 이 목적에 대해 직관적이고 효율적입니다.

### 새 파일에 텍스트 쓰기 또는 기존 파일 덮어쓰기:
```fish
echo "Hello, Fish Shell!" > output.txt
```
이 명령은 `output.txt`에 "Hello, Fish Shell!"을 쓰며, 파일이 없는 경우 생성하거나 이미 있으면 덮어씁니다.

### 기존 파일에 텍스트 추가하기:
현재 내용을 제거하지 않고 기존 파일 끝에 텍스트를 추가하고 싶다면, 추가 연산자 `>>`를 사용하십시오:
```fish
echo "Adding new line to file." >> output.txt
```

### 여러 줄 쓰기:
여러 줄을 파일에 쓰려면 echo와 개행 문자 `\n`을 사용하거나, 세미콜론을 사용해서 여러 echo 명령을 연결할 수 있습니다:
```fish
echo "First Line\nSecond Line" > output.txt
# 또는
echo "First Line" > output.txt; echo "Second Line" >> output.txt
```

### 샘플 출력:
위의 명령을 실행한 후 `output.txt`의 내용을 보려면, `cat` 명령을 사용하십시오:
```fish
cat output.txt
```
```plaintext
First Line
Second Line
```
위와 같이 텍스트를 교체하거나 추가하면 요구사항에 따라 파일 내용을 조작할 수 있으며, Fish Shell에서 텍스트 파일을 다루는 간단하지만 강력한 방법을 보여줍니다.
