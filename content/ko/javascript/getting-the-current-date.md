---
title:    "Javascript: 현재 날짜 가져오기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# 왜 날짜를 알아야 하는가?

컴퓨터 프로그래밍을 하는 사람이라면 지금이 몇 월 몇 일 몇 시며 몇 분인지 알 필요가 있습니다. 특히 웹 개발을 할 때, 쇼핑몰에서 어떤 상품이 판매되는지, 게시판에서 어떤 게시물이 올라오는지 등 기간이나 시간이 중요한 정보를 다루게 됩니다. 이러한 일들은 컴퓨터가 현재 시각을 알도록 프로그래밍하는 데에 있어서 중요한 부분이며, 오늘은 자바스크립트를 사용해 날짜 정보를 다루는 법에 대해 알아보도록 하겠습니다.

# 어떻게 날짜를 다루는가?

Date 객체를 사용하여 자바스크립트에서 현재 날짜를 불러올 수 있습니다. Date 객체는 현재 날짜와 시간에 관련된 다양한 정보를 제공해줍니다. 아래 코드를 참고하여 현재 날짜를 출력해보세요.

```Javascript
let date = new Date(); // 현재 날짜와 시간을 가져옴
let year = date.getFullYear(); // 년도 정보를 가져옴
let month = date.getMonth() + 1; // 월 정보를 가져오며, 0부터 시작하므로 1을 더해줌
let day = date.getDate(); // 일 정보를 가져옴
let hour = date.getHours(); // 시간 정보를 가져옴
let minute = date.getMinutes(); // 분 정보를 가져옴

console.log("오늘은 " + year + "년 " + month + "월 " + day + "일 " + hour + "시 " + minute + "분입니다."); // 출력 결과: 오늘은 2021년 10월 31일 11시 30분입니다.
```

# 깊이 들어가보기

Date 객체는 현재 날짜와 시간을 제공하는 기능 이외에도 다양한 메소드를 지원합니다. 예를 들어, `getDay()` 메소드는 요일에 대한 정보를 숫자로 반환해줍니다. 일요일부터 토요일까지 각각 0부터 6까지의 값을 가집니다. 또한, `toLocaleString()` 메소드를 사용하면 지역 설정에 따라 날짜와 시간이 출력되는 방식을 조정할 수 있습니다. 자세한 내용은 아래 링크를 참고해주세요.

# 참고자료

- [MDN - Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [나무위키 - Date 객체](https://namu.wiki/w/Date%20%EA%B0%9D%EC%B2%B4)
- [Github Gist - Javascript Date 객체 예제](https://gist.github.com/joyrexus/9238467#the-date-object)
- [생활코딩 - Date 객체로 현재 시각 출력하기](https://opentutorials.org/module/3256/19547)