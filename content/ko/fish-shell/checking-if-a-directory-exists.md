---
title:                "디렉토리가 존재하는지 확인하는 방법"
html_title:           "Fish Shell: 디렉토리가 존재하는지 확인하는 방법"
simple_title:         "디렉토리가 존재하는지 확인하는 방법"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍에서 디렉토리가 존재하는지 여부를 확인하는 것은 중요합니다. 이를 통해 적절한 조건문을 작성하고 코드 실행을 제어할 수 있습니다.

## 방법

```Fish Shell```을 사용하여 디렉토리가 존재하는지 여부를 확인하는 방법은 다음과 같습니다.

```fish
if test -d /경로/디렉토리
    echo "디렉토리가 존재합니다."
else
    echo "디렉토리가 존재하지 않습니다."
end
```

위의 예제에서는 ```test``` 명령어를 사용하여 디렉토리의 존재 여부를 확인합니다. 디렉토리가 존재하면 ```-d``` 옵션을 통해 ```true``` 값을 반환하고, 존재하지 않으면 ```false``` 값을 반환합니다. 이를 이용하여 ```if```문을 활용하여 적절한 메시지를 출력하도록 작성할 수 있습니다.

## 깊이 파고들기

보다 깊이 들어가서 디렉토리의 존재 여부를 확인하는 방법을 살펴보겠습니다. ```test``` 명령어는 셸 스크립트에서 자주 사용되는 명령어로, 특정 조건을 평가하고 그 결과를 반환하는 역할을 합니다. 디렉토리의 존재 여부를 확인하는 것 외에도 파일의 존재 여부나, 퍼미션 여부 등 다양한 조건을 확인할 수 있습니다.

또한 ```if```문을 사용하는 것 외에도 ```||``` 나 ```&&``` 연산자를 사용해 조건문을 자세히 제어할 수 있으며, ```-r```, ```-w```, ```-x``` 등의 옵션을 사용하여 퍼미션 여부를 확인할 수 있습니다.

## 관련 정보

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Learn X in Y minutes: Fish](https://learnxinyminutes.com/docs/fish/)