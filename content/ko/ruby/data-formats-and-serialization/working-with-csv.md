---
aliases:
- /ko/ruby/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:20.828798-07:00
description: "\uB8E8\uBE44(Ruby)\uC5D0\uC11C CSV \uD30C\uC77C\uC744 \uB2E4\uB8E8\uB294\
  \ \uAC83\uC740 \uD45C \uD615\uC2DD \uB370\uC774\uD130\uB97C \uCC98\uB9AC\uD558\uB294\
  \ \uC9C1\uAD00\uC801\uC778 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC790\uC8FC \uC774 \uBC29\uBC95\uC744 \uB370\
  \uC774\uD130 \uD30C\uC2F1, \uCD94\uCD9C, \uBCC0\uD658 \uBC0F \uC800\uC7A5\uC744\
  \ \uC704\uD574 \uC0AC\uC6A9\uD558\uBA70, \uC774\uB294 \uB370\uC774\uD130 \uC870\uC791\
  \uC774\uB098 \uBD84\uC11D\uC744 \uB2E4\uB8E8\uB294 \uC791\uC5C5\uC5D0\uC11C \uC911\
  \uC694\uD55C \uAE30\uC220\uC774 \uB429\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:07.060776
model: gpt-4-0125-preview
summary: "\uB8E8\uBE44(Ruby)\uC5D0\uC11C CSV \uD30C\uC77C\uC744 \uB2E4\uB8E8\uB294\
  \ \uAC83\uC740 \uD45C \uD615\uC2DD \uB370\uC774\uD130\uB97C \uCC98\uB9AC\uD558\uB294\
  \ \uC9C1\uAD00\uC801\uC778 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC790\uC8FC \uC774 \uBC29\uBC95\uC744 \uB370\
  \uC774\uD130 \uD30C\uC2F1, \uCD94\uCD9C, \uBCC0\uD658 \uBC0F \uC800\uC7A5\uC744\
  \ \uC704\uD574 \uC0AC\uC6A9\uD558\uBA70, \uC774\uB294 \uB370\uC774\uD130 \uC870\uC791\
  \uC774\uB098 \uBD84\uC11D\uC744 \uB2E4\uB8E8\uB294 \uC791\uC5C5\uC5D0\uC11C \uC911\
  \uC694\uD55C \uAE30\uC220\uC774 \uB429\uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
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
