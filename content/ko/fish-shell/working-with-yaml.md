---
title:                "야믈 사용하기"
html_title:           "Fish Shell: 야믈 사용하기"
simple_title:         "야믈 사용하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜
YAML 파일은 데이터를 효과적으로 저장하고 공유하기 위해 인기 있는 포맷입니다. 여러분이 Fish Shell을 사용한다면, YAML 파일을 다룰 수 있는 방법을 배우는 것이 유용할 것입니다.

## 어떻게
```Fish Shell```에서 YAML 파일을 읽고 쓰는 방법을 간단한 예제와 함께 살펴보겠습니다.
### 읽기
먼저, ```YQ``` 패키지를 설치해야 합니다. 이 패키지는 YAML 파일을 읽고 파싱하는데 도움이 됩니다. 설치는 간단합니다.
```
$ fish
$ fisher install jorgebucaran/yq
```

파일을 읽기 위해서는 ```yq read``` 명령어를 사용합니다. 예를 들어, 다음과 같은 YAML 파일이 있다고 가정해봅시다.
```
# fruits.yml
- name: apple
  price: $2.50
- name: banana
  price: $1.25
```

이제 다음 명령어로 이 파일을 읽을 수 있습니다.
```
$ yq read fruits.yml
```

출력은 다음과 같을 것입니다.
```
- name: apple
  price: $2.50
- name: banana
  price: $1.25
```

### 쓰기
YAML 파일에 새로운 데이터를 추가하고 싶다면 어떻게 해야 할까요? ```yq write``` 명령어를 사용하면 됩니다. 예를 들어 ```fruits.yml```에 새로운 과일 정보를 추가해봅시다.
```
$ yq write fruits.yml - a name: cherry price: $3.00
```

이제 ```fruits.yml```을 다시 읽어보면 새로운 데이터가 추가된 것을 확인할 수 있습니다.
```
- name: apple
  price: $2.50
- name: banana
  price: $1.25
- name: cherry
  price: $3.00
```

## 깊이 파헤치기
YAML 파일에 대해 더 자세히 알아보고 싶다면 다음 자료들을 참고해보세요:
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/)
- [Svetlana Isakova의 "Introduction to YAML"](https://www.infoq.com/articles/yaml-introduction/)
- [YAML 공식 사이트](https://yaml.org/)

## 더 보기
- [YAML 파일 파싱하기](https://github.com/Wind4/vscode-yaml)
- [VS Code에서 Fish Shell 사용하기](https://marketplace.visualstudio.com/items?itemName=couvre.fish-shell-syntax)
- [Fish Shell 공식 레포지토리](https://github.com/fish-shell/fish-shell)