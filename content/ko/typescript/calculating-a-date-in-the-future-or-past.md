---
title:                "TypeScript: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜?
계산된 미래나 과거의 날짜를 알아보는 일은 우리의 일상에서 매우 중요합니다. 이를 통해 예약을 하거나 약속을 잡을 수 있으며, 비즈니스에서도 필수적인 작업입니다.

## 어떻게?
미래나 과거의 날짜를 계산하는 방법은 TypeScript의 내장된 Date 클래스를 사용하는 것입니다. 아래의 코드 예시를 살펴보세요.
```TypeScript
// 오늘 날짜를 기준으로 3일 뒤의 날짜 계산
let today = new Date();
let futureDate = new Date();

// setDate() 함수를 사용하여 현재 날짜에서 3일을 더한 날짜를 계산함
futureDate.setDate(today.getDate() + 3);

// 계산된 날짜를 출력
console.log("3일 후의 날짜:  "+ futureDate.toDateString());
```

위의 코드는 현재 날짜를 기준으로 3일 뒤의 날짜를 계산하는 예시입니다. 이 외에도 각각의 함수를 사용하여 미래나 과거의 날짜를 계산할 수 있습니다.

## 딥 다이브
Date 클래스에는 날짜를 계산하기 위한 다양한 함수들이 있습니다. 이 중에서 가장 많이 사용되는 함수는 setDate(), getDate(), getMonth(), getFullYear()입니다. 이 함수들을 조합하여 계산된 날짜를 원하는 형태로 출력할 수 있습니다.

예를 들어, setDate() 함수는 지정한 날짜를 설정하고, getDate() 함수는 현재 날짜를 반환합니다. 이 두 함수를 조합하여 오늘 날짜에서 3일을 더한 날짜를 구할 수 있고, 이를 getFullYear() 함수와 함께 사용하여 년도를 출력할 수 있습니다.

## 더 알아보기
날짜를 계산하는 데 있어 Date 클래스에 포함된 다양한 함수들이 있습니다. 자세한 내용은 공식 문서를 참고하시기 바랍니다.

# 더 읽어보기
- 공식 TypeScript 문서: https://www.typescriptlang.org/docs/
- MDN Web Docs의 Date 클래스 문서: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date