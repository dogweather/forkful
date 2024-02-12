---
title:                "CSV와 함께 작업하기"
aliases:
- /ko/ruby/working-with-csv/
date:                  2024-02-03T19:21:20.828798-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV와 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

루비(Ruby)에서 CSV 파일을 다루는 것은 표 형식 데이터를 처리하는 직관적인 방법을 제공합니다. 프로그래머들은 자주 이 방법을 데이터 파싱, 추출, 변환 및 저장을 위해 사용하며, 이는 데이터 조작이나 분석을 다루는 작업에서 중요한 기술이 됩니다.

## 어떻게:

루비에는 기본적으로 CSV 라이브러리가 포함되어 있어, CSV 파일에서 읽기 및 쓰기를 간소화합니다. 여기에는 일반적인 작업을 위해 이를 활용하는 방법이 있습니다:

### CSV 파일 읽기
CSV 파일에서 읽으려면, 첫 번째로 CSV 라이브러리가 필요합니다. 그런 다음, 행을 반복하거나 배열로 읽을 수 있습니다.

```ruby
require 'csv'

# 각 행을 배열로 읽기
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# 각 행의 출력 예시: ["data1", "data2", "data3"]
```

### CSV에 쓰기
CSV 파일에 쓰기도 간단합니다. 기존 파일에 추가하거나 새 파일을 만들어 쓸 수 있습니다.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["value1", "value2", "value3"]
end

# 이것은 지정된 헤더와 값들로 'output.csv'를 생성하거나 덮어씁니다.
```

### CSV 문자열 파싱
때로는 문자열에서 직접 CSV 데이터를 파싱해야 할 수도 있습니다. 여기 방법이 있습니다:

```ruby
require 'csv'

data = "name,age,city\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# 예상 출력:
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### SmarterCSV 사용하기
더 복잡한 CSV 작업을 위해서는 `SmarterCSV` 젬이 유용한 도구가 될 수 있습니다. 먼저, 젬을 설치하세요:

```shell
gem install smarter_csv
```

그런 다음, 대용량 파일을 다루거나 더 정교한 파싱 및 조작을 수행하기 위해 이를 사용할 수 있습니다:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# 이것은 'large_data.csv'를 읽고 헤더를 기반으로 각 행을 해시로 출력합니다.
```

요약하자면, 루비의 내장 CSV 라이브러리와 `SmarterCSV`와 같은 타사 젬을 사용하여 CSV 데이터를 처리하는 강력한 지원을 제공하며, 효율적인 데이터 처리 및 조작 작업을 가능하게 합니다.
