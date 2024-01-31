---
title:                "CSV 파일 다루기"
date:                  2024-01-19
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가? 그리고 왜?)
CSV(Comma-Separated Values)는 데이터를 저장하고 교환하기 위한 간단한 파일 형식입니다. 프로그래머는 범용적이고 호환성이 뛰어나기 때문에 CSV를 자주 사용합니다.

## How to (방법)
```Ruby
require 'csv'

# CSV 파일 쓰기
CSV.open("example.csv", "wb") do |csv|
  csv << ["Name", "Age", "City"]
  csv << ["Alice", "30", "Seoul"]
  csv << ["Bob", "22", "Busan"]
end

# CSV 파일 읽기
CSV.foreach("example.csv", headers: true) do |row|
  puts "#{row['Name']} is from #{row['City']}."
end
```
출력:
```
Alice is from Seoul.
Bob is from Busan.
```

## Deep Dive (심층 분석)
CSV 형식은 1970년대부터 사용되어 왔습니다. CSV를 다루기 위해 Ruby는 `csv` 라이브러리를 포함합니다. 엑셀이나 SQL 처럼 데이터베이스와도 호환됩니다. 대안으로 JSON이나 YAML 같은 형식이 있지만, 텍스트 기반의 데이터 교환에는 여전히 CSV가 많이 사용됩니다.

## See Also (참고자료)
- Ruby의 CSV 라이브러리 문서: https://ruby-doc.org/stdlib-2.6/libdoc/csv/rdoc/CSV.html
- CSV 파일 포맷에 대한 더 자세한 정보: https://en.wikipedia.org/wiki/Comma-separated_values
- CSV 데이터를 다루는 다른 언어들의 라이브러리 비교: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
