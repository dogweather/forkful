---
title:                "TypeScript: 날짜를 문자열로 변환하기"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

"## 왜" 

날짜를 문자열로 변환하는 작업에 참여하는 이유를 간략히 설명합니다. 

## 왜
여러분은 TypeScript를 사용하면서 타입 변환 작업을 자주 할 수 있습니다. 이 중에서도 날짜 객체를 문자열로 변환하는 작업은 매우 중요합니다. 예를 들어, 사용자의 생일이나 이벤트 날짜를 문자열 형태로 저장해야하는 경우가 있을 수 있습니다. 따라서 우리는 TypeScript에서 날짜를 문자열로 변환하는 방법을 알아보겠습니다.

"## 사용방법"

아래 코드 블록을 참고하여 TypeScript에서 날짜를 문자열로 변환하는 방법을 살펴보세요. 

```TypeScript
let today: Date = new Date();
let dateString: string = today.toDateString();
console.log(dateString);
```
output: Fri Sep 11 2020

위의 코드를 실행하면 현재 날짜를 문자열로 변환한 결과를 볼 수 있습니다. 이렇게 변환된 날짜 문자열은 다양한 형태로 사용할 수 있습니다. 

따라서 날짜를 원하는 형태로 변환하려면 Date 객체에서 제공하는 다양한 메소드를 활용해야 합니다. 예를 들어, 년도, 월, 일 등을 포함한 원하는 형태로 날짜를 변환할 수 있습니다. 아래의 코드 예제를 참고하시기 바랍니다.

```TypeScript
let today: Date = new Date();
let options: Intl.DateTimeFormatOptions = { year: 'numeric', month: 'long', day: 'numeric' };
let dateString: string = today.toLocaleDateString(undefined, options);
console.log(dateString);
```
output: September 11, 2020 

위의 코드에서는 toLocaleDateString() 메소드를 사용하여 원하는 형태로 날짜를 변환하였습니다. 마이크로소프트에서 제공하는 자세한 문서를 참고하여 필요에 맞게 원하는 형태로 날짜를 변환할 수 있습니다.


"## 깊이 있는 내용"

날짜를 문자열로 변환하는 작업은 매우 흔하고 중요한 작업입니다. 따라서 이번 섹션에서는 이 작업에 대해 깊이 있는 내용을 알아보겠습니다. 

우선 Date 객체는 자바스크립트에서 날짜와 시간을 다루는 데 사용되는 기본 객체입니다. 이 객체는 다양한 메소드를 제공하며 날짜와 시간을 다양한 방식으로 조회하고 조작할 수 있습니다. 따라서 우리는 이 객체를 사용하여 날짜를 문자열로 변환하는 작업을 수행할 수 있습니다. 

또한 날짜를 변환할 때는 어떤 형식으로 변환할지 정하는 옵션을 설정할 수 있습니다. 예를 들어, ‘year', 'month', 'day'와 같은 옵션을 지정하여 날짜를 년, 월, 일로 나누어 변환할 수 있습니다. 이러한 옵션을 통해 우리는 Date 객체에서 제공하는 다양한 메소드를 조합하여 정확한 날짜를 원하는 형태로 변환할 수 있습니다. 

받은 날짜 객체를 어떤 형식으로 변환할지 선택함으로써 우리는 날짜를 손쉽게 문자열로 변환할 수 있습니다. 따라서 TypeScript에서는 다양한 날짜 변환 메소드를 사용하여 원하는 형태로 날짜를 변환할 수 있습니다.


"## 참고