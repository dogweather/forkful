---
title:                "CSV 파일 다루기"
html_title:           "Elixir: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
CSV로 작업하는 것은 데이터를 텍스트 형식으로 저장하거나 전송하는 데에 사용되는 방법입니다. 프로그래머들은 데이터를 관리하고 조작할 때 CSV 포맷을 사용합니다.

## 방법:
```Elixir
# 인코딩된 CSV 파일을 읽기
{:ok, data} = File.read("example.csv")
CSV.decode(data)

# 데이터를 CSV 형식으로 변환하기
content = [["Name", "Age"], ["John", 28], ["Sarah", 24]]
CSV.encode(content)
```

## 깊이 들어가보기:
CSV는 1970년대부터 사용되어 왔으며 데이터를 쉽게 저장하고 전송하며 공유할 수 있는 포맷으로 널리 알려져 있습니다. CSV로 작업하는 대안으로는 JSON, XML, 혹은 텍스트 형식 등이 있습니다. Elixir에서는 Erlang 라이브러리인 `:csv`를 사용하여 CSV 파일을 다룰 수 있습니다.

## 출처 알아보기:
- [Elixir 문서: `:csv` 모듈](https://hexdocs.pm/elixir/csv.html)
- [나무위키: CSV](https://namu.wiki/w/CSV)