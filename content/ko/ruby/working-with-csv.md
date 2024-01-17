---
title:                "CSV 작업하기"
html_title:           "Ruby: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜?: 
CSV를 다루는 것은 텍스트 파일로 데이터를 저장하고 읽는 것을 말합니다. 프로그래머들은 CSV를 사용하는 이유는 간단합니다. 이것은 텍스트 데이터를 다루는 데 가장 일반적인 방식이기 때문입니다.

## 사용 방법:
다음 코드 블록에는 CSV 작업에 대한 기초적인 예와 샘플 출력이 포함되어 있습니다.

```Ruby 
require 'csv'
# 파일 쓰기
CSV.open("example.csv", "w") do |csv|
    csv << ["Name", "Age", "Occupation"]
    csv << ["John", "25", "Programmer"]
    csv << ["Jane", "30", "Designer"]
end

# 파일 읽기
CSV.foreach("example.csv") do |row|
    puts "#{row[0]} is a #{row[2]} who is #{row[1]} years old."
end
```

출력:
```
John is a Programmer who is 25 years old.
Jane is a Designer who is 30 years old.
```

## 깊이 있는 정보:
CSV는 Comma Separated Values의 약자로써, 과거에는 데이터를 컴퓨터에서 인식하기 쉬운 형식으로 저장하기 위해 개발되었습니다. 현재에는 다양한 데이터 형식이 있지만 여전히 많은 프로그래머들이 CSV를 사용하는 이유 중 하나는 간단하고 읽기 쉬운 구조 때문입니다. 또한, CSV 파일을 가져와서 다른 형식으로 변환하는 도구들도 많이 존재합니다.

## 관련 자료:
- [Ruby CSV 문서](https://ruby-doc.org/stdlib/libdoc/csv/rdoc/CSV.html)
- [CSV vs XML vs JSON](https://stackoverflow.com/questions/911195/csv-vs-xml-vs-json-which-is-the-best-for-mobile-apps)
- [CSV와 관련된 라이브러리들](https://www.ruby-toolbox.com/categories/government)