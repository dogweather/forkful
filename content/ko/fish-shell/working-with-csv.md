---
title:                "csv 파일 작업하기"
html_title:           "Fish Shell: csv 파일 작업하기"
simple_title:         "csv 파일 작업하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
CSV란 무엇일까요? CSV는 Comma-Separated Values의 약자로, 쉼표로 구분된 데이터를 담는 파일 형식입니다. 프로그래머들은 CSV 파일을 사용하는 이유는 데이터를 쉽게 읽고 작업할 수 있기 때문입니다.

## 방법:
```Fish Shell```에서 CSV 파일을 다루는 방법은 다음과 같습니다. 먼저, ```read``` 명령어를 사용하여 CSV 파일을 읽고 변수에 저장합니다. 그리고 변수 값을 ```echo``` 명령어를 사용해서 출력하거나, ```for``` 반복문을 사용해서 각각의 데이터 값을 처리할 수 있습니다.

```fish
set csv (read --from=csv_file.csv)
echo $csv
for data in $csv
	echo $data
end
```

## 깊게 들어가보기:
CSV 파일 형식은 1980년대부터 사용되어 온 고전적인 형식입니다. 하지만 요즘에는 더 발전된 형식인 JSON도 많이 사용됩니다. 또한, CSV 파일을 처리하기 위해 별도의 프로그래밍 언어나 라이브러리를 사용할 필요가 없이, ```Fish Shell```에서 손쉽게 처리할 수 있습니다.

## 더 알아보기:
- https://en.wikipedia.org/wiki/Comma-separated_values
- https://fishshell.com/docs/current/commands.html#read
- https://fishshell.com/docs/current/commands.html#echo