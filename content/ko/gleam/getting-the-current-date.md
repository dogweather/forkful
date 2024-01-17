---
title:                "현재 날짜 가져오기"
html_title:           "Gleam: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

### 지금은 무엇이며 왜?: 

현재 날짜를 얻는 것은 일반적으로 프로그래머들이 자주 사용하는 기능입니다. 이는 현재 시간에 따라 프로그램의 동작을 조정하는 데 사용되거나 날짜와 시간에 대한 정보를 저장하고 표시하는 데 사용될 수 있기 때문입니다.

### 방법:

```Gleam
import time
Gleam.time.now
```
```
2021-08-23T22:44:19.3908
```

```Gleam
import time
Gleam.time.now(date_format="%Y년 %m월 %d일")
```
```
2021년 08월 23일
```

### 깊이 들어가기:

현재 날짜를 얻는 기능은 거의 모든 프로그래밍 언어에서 제공됩니다. 이는 운영체제에서 시간과 날짜를 관리하기 때문입니다. 대부분의 언어에서는 시간과 날짜를 다루는 라이브러리를 제공하며, Gleam 역시 time 라이브러리를 제공합니다.

현재 날짜를 얻는 방법에는 여러 가지가 있습니다. 예를 들어 유닉스 시간(epoch time)을 얻는 방법과 다양한 형식으로 날짜를 표시하는 방법 등이 있습니다.

대부분의 경우 날짜 및 시간을 다루는 라이브러리를 사용하는 것이 가장 간단하고 효율적인 방법입니다. 그러나 프로그래머는 필요에 따라 다른 방법을 사용할 수 있으며, Gleam은 여러 가지 옵션을 제공하여 유연성을 높입니다.

### 관련 자료:

- [Gleam time 라이브러리 문서](https://gleam.run/modules/gleam_stdlib#time)
- [날짜 형식 지정 방법](https://docs.gleam.run/language/formats#dates)