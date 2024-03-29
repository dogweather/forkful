---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:51.915954-07:00
description: "Fish Shell\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC5D0 \uAE30\uB85D\
  \uD558\uB294 \uAC83\uC740 \uB370\uC774\uD130\uB97C \uC9C0\uC18D\uC801\uC73C\uB85C\
  \ \uC800\uC7A5\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uC5B4, \uB3D9\uC77C\uD55C\
  \ Fish \uC2A4\uD06C\uB9BD\uD2B8 \uB610\uB294 \uB2E4\uB978 \uD504\uB85C\uADF8\uB7A8\
  \uC5D0 \uC758\uD55C \uB370\uC774\uD130 \uAC80\uC0C9\uC774\uB098 \uC870\uC791\uC744\
  \ \uC27D\uAC8C \uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uB85C\uAE45, \uC124\uC815 \uC800\uC7A5, \uB610\
  \uB294 \uCD94\uAC00 \uCC98\uB9AC\uB97C \uC704\uD55C \uB370\uC774\uD130 \uB0B4\uBCF4\
  \uB0B4\uAE30\u2026"
lastmod: '2024-03-13T22:44:55.882227-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC5D0 \uAE30\uB85D\
  \uD558\uB294 \uAC83\uC740 \uB370\uC774\uD130\uB97C \uC9C0\uC18D\uC801\uC73C\uB85C\
  \ \uC800\uC7A5\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uC5B4, \uB3D9\uC77C\uD55C\
  \ Fish \uC2A4\uD06C\uB9BD\uD2B8 \uB610\uB294 \uB2E4\uB978 \uD504\uB85C\uADF8\uB7A8\
  \uC5D0 \uC758\uD55C \uB370\uC774\uD130 \uAC80\uC0C9\uC774\uB098 \uC870\uC791\uC744\
  \ \uC27D\uAC8C \uD560 \uC218 \uC788\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uB85C\uAE45, \uC124\uC815 \uC800\uC7A5, \uB610\
  \uB294 \uCD94\uAC00 \uCC98\uB9AC\uB97C \uC704\uD55C \uB370\uC774\uD130 \uB0B4\uBCF4\
  \uB0B4\uAE30\u2026"
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜?

Fish Shell에서 텍스트 파일에 기록하는 것은 데이터를 지속적으로 저장할 수 있게 해주어, 동일한 Fish 스크립트 또는 다른 프로그램에 의한 데이터 검색이나 조작을 쉽게 할 수 있게 합니다. 프로그래머들은 이를 로깅, 설정 저장, 또는 추가 처리를 위한 데이터 내보내기 등의 목적으로 사용합니다.

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
