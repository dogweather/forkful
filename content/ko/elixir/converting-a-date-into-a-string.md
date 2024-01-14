---
title:    "Elixir: 날짜를 문자열로 변환하기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Elixir로 날짜를 문자열로 변환하는 이유  
Elixir는 다양한 데이터 유형을 처리할 수 있는 강력한 언어입니다. 따라서 Elixir 프로그래머라면 날짜를 문자열로 변환해야 할 일이 생길 수 있습니다. 이번 블로그 포스트에서는 날짜를 문자열로 변환하는 방법과 관련된 깊은 정보를 살펴보겠습니다. 

## 왜 
날짜를 문자열로 변환해야 하는 이유는 다양합니다. 예를 들어, 특정 날짜를 데이터베이스에 저장하기 위해 문자열 형식으로 변환해야 할 수도 있습니다. 또는 사용자에게 보여지는 날짜를 문자열로 표시해야 할 수도 있습니다. 이러한 경우에 날짜를 문자열로 변환해주는 기능은 매우 유용합니다.

## 어떻게 
Elixir에서 날짜를 문자열로 변환하는 방법은 매우 간단합니다. 우선, ```~D[YYYY-MM-DD]``` 을 사용하여 원하는 날짜 형식을 지정합니다. 그리고 ```to_string/2``` 함수를 사용하여 날짜를 문자열로 변환합니다. 아래의 예제 코드를 살펴보면서 자세히 살펴보겠습니다.

```elixir
date = ~D[2020-12-25]
formatted_date = to_string(date, "~D[YYYY-MM-DD]")
IO.puts(formatted_date)
```
```
2020-12-25
```

위의 예제에서는 ```~D[YYYY-MM-DD]``` 형식으로 날짜를 지정하고, ```to_string/2``` 함수를 사용하여 날짜를 문자열로 변환하였습니다. 이렇게 간단하게 날짜를 문자열로 변환할 수 있습니다.

## 깊이 들어가기 
날짜를 문자열로 변환하는 과정에서 발생할 수 있는 오류를 방지하기 위해 날짜 형식을 지정하는 방법에 대해 더욱 깊이 들어가보겠습니다. Elixir에서는 우선순위가 높은 형식을 사용하여 날짜를 처리합니다. 예를 들어, ```to_string(date, "~D[YYYY-MM-DD]")``` 코드에서는 ```YYYY-MM-DD``` 형식이 우선시되지만, ```to_string(date, "~D[YYYY-DD-MM]")``` 코드에서는 ```YYYY-DD-MM``` 형식이 우선시되어 이상한 날짜 형식이 출력됩니다. 따라서 정확한 날짜 형식을 지정하는 것이 중요합니다.

## 참고 자료 
[Elixir Datetime](https://hexdocs.pm/elixir/DateTime.html)  
[Elixir Date](https://hexdocs.pm/elixir/Date.html)  
[Elixir String](https://hexdocs.pm/elixir/String.html)  

## 참고 자료 
[Elixir Datetime](https://hexdocs.pm/elixir/DateTime.html)  
[Elixir Date](https://hexdocs.pm/elixir/Date.html)  
[Elixir String](https://hexdocs.pm/elixir/String.html)