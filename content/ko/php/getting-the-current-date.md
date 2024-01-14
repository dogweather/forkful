---
title:    "PHP: 현재 날짜 가져오기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜

날짜를 불러오는 것에 대해 이야기하기 전에 왜 이 작업을 해야 하는지에 대해 생각해보겠습니다. 웹 프로그래밍에서 일반적으로 현재 날짜와 시간을 사용하는 이유는 다양합니다. 예를 들어, 사용자에게 현재 날짜와 시간을 보여주는 메시지를 표시하거나 스케줄러를 작동시켜 특정 시간에 작업을 실행하는 등 많은 용도로 사용됩니다.

## 방법

PHP에서 현재 날짜와 시간을 불러오기 위해서는 `date()` 함수를 사용해야 합니다. 이 함수는 첫 번째 매개변수로 형식을 지정할 수 있습니다. 다음 예제는 "년-월-일 시간:분:초" 형식으로 현재 날짜와 시간을 출력하는 코드입니다.

```PHP
$date = date("Y-m-d H:i:s");
echo $date;
```

출력은 다음과 같이 나타납니다.

```
2020-07-15 09:30:00
```

## 깊은 곳으로

PHP에서 `date()` 함수는 내부적으로 `time()` 함수를 사용합니다. 이 함수는 현재 시간을 Epoch 시간으로 나타낸 값을 반환합니다. Epoch 시간은 1970년 1월 1일 자정 이후의 초를 나타내며 다양한 용도로 사용됩니다. `date()` 함수에서 불러오는 날짜와 시간은 이 값을 가지고 변환하여 출력하는 것입니다.

## 참조하기

- PHP `date()` 함수에 대한 더 자세한 정보: [PHP Manual - Date and Time Functions](https://www.php.net/manual/en/function.date.php)
- Epoch 시간에 대한 더 자세한 정보: [Wikipedia - Epoch Time](https://en.wikipedia.org/wiki/Unix_time)
- PHP와 날짜/시간 처리에 대한 깊은 이해: [Learn PHP Date & Time](https://www.w3schools.com/php/php_date.asp)