---
title:                "CSV 작업하기"
html_title:           "Lua: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
CSV는 Comma-Separated Values의 약자로, 쉼표로 구분된 데이터를 의미합니다. 프로그래머들은 CSV 파일 형식을 사용하여 데이터를 저장하고 처리하는 등 다양한 분야에서 활용합니다.

## 어떻게:
```
-- CSV 파일 읽기
local csv = require("csv")
local file = csv.open("example.csv") -- 파일 경로 지정
for fields in file:lines() do -- 각 라인마다 필드를 읽어옴
  print(fields[1], fields[2]) -- 필드 출력
end
file:close() -- 파일 닫기
```

```
-- CSV 파일 쓰기
local csv = require("csv")
local file = csv.open("example.csv", "w") -- 파일 경로 지정, 쓰기 모드
file:write({"Name", "Age"}) -- 첫 줄에 필드명 추가
file:write({"John", 25}) -- 데이터 추가
file:write({"Mary", 28})
file:close() -- 파일 닫기
```

```
-- CSV 문자열 파싱
local csv = require("csv")
local str = "John,25\nMary,28" -- 문자열 형식의 CSV 데이터
local records = csv.parse(str) -- 데이터 파싱
for i, record in ipairs(records) do
  -- 각 레코드 출력
  print(record[1], record[2])
end
```

## 깊이 파고들기:
CSV는 1970년대에 탄생한 데이터 형식으로, 기존의 테이블 형식 데이터보다 더 간편하게 저장하고 처리할 수 있어서 널리 사용됩니다. 다른 대안으로는 XML, JSON 등의 형식이 있지만, CSV는 간단하고 가볍기 때문에 데이터 크기가 큰 경우에 유용합니다. CSV를 다루는 방법은 간단하지만, 데이터의 유효성 검사 및 문제 해결을 위해서는 문서를 참조하는 것이 좋습니다.

## 관련 자료:
- [Lua에서 CSV 처리하기](https://web.archive.org/web/20210119165520/https://jrgraphix.net/r/Unicode/0020-007F)
- [CSV 형식 소개](https://web.archive.org/web/20210119165625/https://www.passeidireto.com/arquivo/3810495/java-io-streams)