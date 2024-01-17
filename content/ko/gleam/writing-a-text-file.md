---
title:                "텍스트 파일 작성하기"
html_title:           "Gleam: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
텍스트 파일을 작성하는 것은 말 그대로 텍스트를 적는 것을 의미합니다. 프로그래머들은 주로 이를 수행하는 이유는 데이터를 저장하고 관리하기 위해서입니다.

## 방법:
```Gleam
import gleam/io

// 텍스트 파일 생성
let result =
    file.write({path="/my-file.txt", contents="안녕하세요!"})

// 텍스트 파일 내용 읽기
let result =
    file.read("/my-file.txt")

// 텍스트 파일에 라인 추가하기
file.append({path="/my-file.txt", contents="잘가요!"})
```

## 심층 분석:
(1) 텍스트 파일 작성의 역사적 배경은 다음과 같습니다. (2) 대안으로는 데이터베이스 등 다른 형태의 데이터 저장 방식이 있습니다. (3) 텍스트 파일 작성의 구현 방법은 파일 시스템을 통해 데이터를 디스크에 쓰는 과정을 포함합니다.

## 관련 자료:
- [Gleam 문서](https://gleam.run/)
- [파일 시스템 모듈 문서](https://gleam.run/modules/io/)