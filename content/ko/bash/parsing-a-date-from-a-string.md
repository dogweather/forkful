---
title:                "문자열에서 날짜 분석하기"
html_title:           "Bash: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

일정을 문자열로부터 추출하는 일이 바로 문자열 해석(parsing)입니다. 프로그래머들은 이를 하게 됩니다. 왜냐하면 날짜로 추출할 수 있는 다양한 데이터에서 실제 날짜 정보를 불러오기 위해서입니다. 

## What & Why? 

날짜 데이터는 프로그래밍에 있어서 매우 중요합니다. 예를 들어, 각각의 이벤트를 날짜별로 정리하거나 기간을 정하는 등의 다양한 작업에 필수적으로 사용되는 데이터입니다. 하지만 때로는 이 데이터가 문자열 형태로만 제공되는 경우가 있습니다. 이 때문에 적절한 형식으로 데이터를 추출하고 싶을 때 문자열 해석이 필요합니다. 

## How to: 

여러분의 Bash 셸에서 다음과 같이 입력하여 코드를 작성하세요. 

```Bash 
# 날짜 데이터 추출 예제 

# 원본 문자열
date_string="2020-01-01"

# 추출하고 싶은 연도 정보만 오른쪽으로부터 총 해당하는 길이만큼 잘라냅니다.
year=${date_string:0:4}
echo ${year}  # 2020 출력 

# 원하는 날짜 형식으로 바꾸고 싶을 때는 'date' 명령어를 사용합니다.
formatted_date=$(date -d ${date_string} +"%m/%d/%Y")
echo ${formatted_date}  # 01/01/2020 출력 
```

## Deep Dive: 

날짜 문자열 추출의 역사적 배경으로는 컴퓨터 시스템에서 날짜 정보를 표현하는 방식이 점차 발전하며 현재에 이르게 됐습니다. 최근에는 핸드폰 등의 장치에서 사용하는 형태로, `YYYY-MM-DD`와 같은 숫자 형식으로 표현되는 것이 일반적이지만 이전에는 `MM/DD/YYYY`와 같이 다른 형식으로 표현되기도 했습니다. 현재에는 `date` 명령어를 사용하여 여러가지 형식으로 날짜를 바꿀 수 있기 때문에 문자열로부터 추출하는 방식도 더욱 다양해졌습니다. 

날짜 데이터를 문자열 형태로 사용할 때는 수동으로 추출하는 것보다 `date` 명령어나 관련 라이브러리를 사용하는 것이 더 좋은 방법입니다. `date` 명령어를 이용한 `man` 페이지를 확인하는 것도 도움이 될 수 있습니다. 

## See Also: 

- [GNU Coreutils Manual: Date input formats](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html)