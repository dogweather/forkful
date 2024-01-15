---
title:                "CSV 파일 작업하기"
html_title:           "Ruby: CSV 파일 작업하기"
simple_title:         "CSV 파일 작업하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

CSV 파일을 다루는 것은 데이터를 쉽게 읽고 조작할 수 있기 때문입니다. 이를 통해 사용자는 데이터를 더 잘 관리하고 분석하고 활용할 수 있습니다.

## 코딩하는 방법

Ruby에서 CSV 파일을 다루는 방법은 간단합니다. CSV 모듈을 ```require``` 하고 ```CSV.foreach``` 또는 ```CSV.read```를 사용하여 파일을 읽습니다. 데이터를 조작하고 저장하는 방법은 각각 다릅니다.

```Ruby
#csv 파일 읽기 예제
require 'csv'

CSV.foreach("data.csv", headers: true) do |row|
    puts row
end

#csv 파일 조작 예제
require 'csv'

data = CSV.read("data.csv", headers: true)
data.each do |row|
    row["age"] += 1  #나이를 1 더하여 업데이트
end
CSV.open("new_data.csv", "w") do |csv|
    data.each do |row|
        csv << row  #새로운 파일에 데이터 저장
    end
end
```

위 코드는 csv 파일을 읽고 조작하는 간단한 예제입니다. 각각의 캐러처를 반복문으로 접근하여 데이터를 수정하고 새로운 파일에 저장할 수 있습니다.

## 깊게 알아보기

CSV 모듈은 다양한 옵션을 제공하며 사용자가 데이터를 더욱 세밀하게 다룰 수 있도록 해줍니다. 예를 들어, ```CSV.foreach``` 에서 ```headers: true``` 옵션을 사용하면 첫 번째 줄을 헤더로 인식하여 열 제목을 이용할 수 있습니다. 또는 ```CSV.read``` 에서 ```headers: true``` 옵션과 함께 ```converters: :numeric``` 옵션을 사용하면 숫자로 변환된 데이터를 얻을 수 있습니다.

또한, CSV 모듈은 데이터를 처리하는 속도가 빠르기 때문에 대용량의 데이터도 빠르게 처리할 수 있습니다. 따라서 데이터 분석 및 처리에 유용하게 사용할 수 있습니다.

## 더 알아보기

CSV 모듈을 사용하여 데이터를 다루는 방법에 대해 더 알아보려면 다음 링크를 참고하시기 바랍니다.

[CSV 모듈 문서](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html)

[CSV 모듈 사용 예제](https://www.rubyguides.com/2018/10/parse-csv-ruby/)

## 참고 링크

[Ruby 공식 사이트](https://www.ruby-lang.org/ko/)

[Ruby 커뮤니티 사이트](https://www.ruby-forum.com/)