---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가? 
현재 날짜를 얻는 것은 컴퓨터에서 현재의 시간을 표시하거나 추적하기 위한 과정입니다. 프로그래머들은 이 기능을 이용하여 작업의 타임스탬프를 생성하거나, 사용자에게 실시간 정보를 보여주기 위해 사용합니다.

## 실행 방법:
TypeScript에서 현재 날짜를 얻는 가장 간단한 방법은 다음의 코드처럼 새로운 Date 객체를 생성하는 것입니다:

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```
이 코드를 실행하면, 현재 날짜와 시간이 표시됩니다.

## 심화 학습
일찍이, 컴퓨터 시스템에서는 시간을 수치로 변환하여 처리했습니다. Unix 시간 ("UNIX timestamp")은 이러한 예로, 1970년 1월 1일부터 세기마다 숫자가 증가합니다. 

다른 방법으로는 'moment.js'와 같은 라이브러리를 활용할 수 있지만, 최근에는 JavaScript의 Date 객체의 표준화로 인해 이러한 라이브러리의 필요성이 줄고 있습니다.

'new Date()' 는 JavaScript의 클래스이며, TypeScript에서는 이를 그대로 사용합니다. 이 내부에서는 현재 시스템의 로케일 설정과 시간대에 따라 결과가 달라질 수 있습니다.

## 참조 링크
- [MDN Web Docs - Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [moment.js 라이브러리](https://momentjs.com/)
- [UNIX 타임스탬프 계산기](https://www.unixtimestamp.com/index.php)