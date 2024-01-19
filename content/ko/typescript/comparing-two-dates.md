---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜 두 개를 비교하는 것은 그들이 무엇보다 먼저 오는지 아닌지를 파악하는 과정입니다. 프로그래머들은 이를 통해 시간 경과에 따른 상태 변경, 기능 트리거 등을 관리합니다.

## 어떻게:
TypeScript에서 날짜를 비교하는 방법은 아래 예제를 통해 확인하실 수 있습니다.

```TypeScript
let date1 = new Date(2021, 1, 1);
let date2 = new Date(2021, 1, 2);

//날짜를 밀리초로 ​​변환하여 비교
if(date1.getTime() > date2.getTime()) {
    console.log("Date1이 Date2보다 늦습니다");
} else {
    console.log("Date2가 Date1보다 늦습니다");
}
```

위 코드를 실행하면 다음 출력이 출력됩니다:

```
Date2가 Date1보다 늦습니다
```

## 자세히 알아보기:
날짜 비교는 개발 초기부터 중요한 과제였으며, 시간 경과 기능이 필요한 원래의 대형 메인프레임 시스템에서부터 다루어져 왔습니다. Date() 생성자를 사용하면 JavaScript와 TypeScript에서 쉽게 날짜를 비교할 수 있습니다. 

안타깝게도, 이 방법에는 날짜의 시간 부분을 다루는 경우 오류가 발생할 수 있습니다. 
이러한 문제를 해결하는 대안은 특정 시간을 기준으로 한 'moment.js'라는 라이브러리를 사용하는 것입니다. 

```TypeScript
import * as moment from 'moment';

let date1 = moment('2021-01-01');
let date2 = moment('2021-01-02');

if(date1.isAfter(date2)) {
    console.log("Date1이 Date2보다 늦습니다");
} else {
    console.log("Date2가 Date1보다 늦습니다");
}
```
## 추가로 참고하기:
더 자세한 정보 및 관련 자료는 아래 링크에서 확인하실 수 있습니다:

- JavaScript Date 객체: https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date
- moment.js 라이브러리: https://momentjs.com/