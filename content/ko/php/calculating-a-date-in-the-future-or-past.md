---
title:    "PHP: 미래 또는 과거 날짜 계산하기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 데 참여하는 이유는 무엇인가요? 이것은 코드에서 특정 날짜를 동적으로 처리하기 위해 매우 유용합니다.

## 방법

우리는 PHP에서  ```strtotime()``` 함수를 사용하여 미래나 과거의 날짜를 계산할 수 있습니다. 다음은 "2020-01-01"을 기준으로 일주일 뒤의 날짜를 계산하는 코드 예제입니다.

```PHP
<?php 
$date = "2020-01-01";
echo date("Y-m-d", strtotime($date. " + 1 week")); 
```

위 코드의 출력은 2020-01-08이 될 것입니다. 또한 "Last Monday"와 같은 상대적인 날짜도 계산할 수 있습니다. 이 경우에도 ```strtotime()``` 함수를 사용하면 됩니다.

자 이제 우리는 다양한 계산 방법을 알았으니 날짜를 동적으로 처리하는 데에 더 이상 어려움이 없을 것입니다.

## 심층 분석

그러나 실제로 날짜를 계산하는 방법이 어떻게 되는지 궁금하지 않으신가요? 우리는 [UNIX time](https://ko.wikipedia.org/wiki/UNIX_%ED%91%9C%EC%A4%80%EC%9B%90)이라는 개념을 사용하여 날짜를 표현합니다. 이는 협정 세계시(UTC) 1970년 1월 1일 00:00:00를 기준으로 경과한 초수로 날짜를 나타내는 방식입니다.

```strtotime()``` 함수는 입력된 날짜를 UNIX time으로 변환하고, 이후에 사용자가 원하는 날짜 형식으로 변환하여 반환합니다. 이러한 개념을 알면 날짜를 처리하는데에 더 유연하게 코드를 짤 수 있게 될 것입니다.

## 참고 자료

- [PHP String to Timestamp strtotime()](https://www.w3schools.com/php/func_date_strtotime.asp)
- [PHP Date/Time Functions](https://www.php.net/manual/en/ref.datetime.php)
- [What is Unix Time?](https://www.epochconverter.com/epoch/what-is-epoch.php)