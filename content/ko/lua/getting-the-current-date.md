---
title:                "현재 날짜 받기"
html_title:           "Lua: 현재 날짜 받기"
simple_title:         "현재 날짜 받기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

Lua에서 현재 날짜 받아오기

## 무엇 & 왜?

현재 날짜 받아오기란 무엇일까요? 간단하게 말하자면 컴퓨터가 현재의 날짜와 시간을 알려주는 것입니다. 프로그래머들은 이 기능을 사용하여 프로그램을 작성하거나 시스템 설정을 변경할 때 유용하게 활용합니다.

## 사용 방법:

현재 날짜를 받아오는 방법은 간단합니다. 다음과 같이 코드를 입력해보세요:

```Lua 
currentDate = os.date("%x")  
print(currentDate) 
```

위 코드를 실행하면 현재 날짜가 yyyy/mm/dd 형태로 출력될 것입니다.

또한, 시간 정보를 함께 받아오고 싶다면 다음과 같이 코드를 변경할 수 있습니다:

```Lua
currentTime = os.date("%x %X")
print(currentTime)
```

위 코드를 실행하면 yyyy/mm/dd hh:mm:ss 형태로 현재 날짜와 시간이 출력될 것입니다.

## 깊게 파고들기:

현재 날짜를 받아오는 기능은 Lua의 표준 라이브러리에 포함되어 있습니다. 그렇기 때문에 별도의 설치 과정 없이 즉시 사용할 수 있습니다. 하지만, 다른 언어를 사용하는 프로그래머라면 이와 비슷한 기능을 유사한 방법으로 사용할 수 있습니다. 예를 들어 파이썬에서는 datetime 라이브러리를 사용해 날짜를 받아올 수 있습니다.

## 관련 자료:

- Lua 공식 문서: [os.date()](https://www.lua.org/manual/5.3/manual.html#pdf-os.date)
- 파이썬 공식 문서: [datetime](https://docs.python.org/3/library/datetime.html)