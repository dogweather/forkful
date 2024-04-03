---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:28.021229-07:00
description: "\uBC29\uBC95: Fish Shell\uC740 YAML \uD30C\uC2F1\uC744 \uB0B4\uC7A5\
  \ \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC9C0\uB9CC, `yq`(\uAC00\uBCBC\uC6B0\uBA74\uC11C\
  \ \uC774\uC2DD \uAC00\uB2A5\uD55C \uCEE4\uB9E8\uB4DC\uB77C\uC778 YAML \uD504\uB85C\
  \uC138\uC11C) \uAC19\uC740 \uC81C3\uC790 \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD574 YAML\
  \ \uB370\uC774\uD130\uB97C \uCC98\uB9AC\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. **yq\
  \ \uC124\uCE58\uD558\uAE30(\uBBF8\uC124\uCE58\uC2DC):**."
lastmod: '2024-03-13T22:44:55.884959-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\uC740 YAML \uD30C\uC2F1\uC744 \uB0B4\uC7A5 \uC9C0\uC6D0\uD558\
  \uC9C0 \uC54A\uC9C0\uB9CC, `yq`(\uAC00\uBCBC\uC6B0\uBA74\uC11C \uC774\uC2DD \uAC00\
  \uB2A5\uD55C \uCEE4\uB9E8\uB4DC\uB77C\uC778 YAML \uD504\uB85C\uC138\uC11C) \uAC19\
  \uC740 \uC81C3\uC790 \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD574 YAML \uB370\uC774\uD130\
  \uB97C \uCC98\uB9AC\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 방법:
Fish Shell은 YAML 파싱을 내장 지원하지 않지만, `yq`(가벼우면서 이식 가능한 커맨드라인 YAML 프로세서) 같은 제3자 도구를 사용해 YAML 데이터를 처리할 수 있습니다.

**yq 설치하기(미설치시):**
```fish
sudo apt-get install yq
```

**YAML 파일에서 값 읽기:**
예를 들어, 다음과 같은 내용의 `config.yaml` 파일이 있다고 가정합시다:
```yaml
database:
  host: localhost
  port: 3306
```

데이터베이스 호스트를 읽으려면, 다음을 사용합니다:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**샘플 출력:**
```
localhost
```

**YAML 파일 내 값 업데이트하기:**
`port`를 `5432`로 업데이트 하려면, 사용합니다:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**업데이트 확인:**
```fish
yq e '.database.port' config.yaml
```
**샘플 출력:**
```
5432
```

**새 YAML 파일 작성하기:**
미리 정의된 내용으로 새로운 `new_config.yaml`을 생성하려면:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
이는 `yq`를 사용하여 문자열을 처리하고 새 YAML 파일로 예쁘게 출력하는(-P 플래그) 것을 사용합니다.

**복잡한 구조 파싱하기:**
더 복잡한 YAML 파일이 있고, 중첩된 배열이나 객체를 추출해야 한다면, 다음을 사용할 수 있습니다:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**샘플 출력:**
```
server1
server2
```
`yq`를 사용하여, Fish Shell은 YAML 문서를 탐색하고 다양한 자동화 및 구성 작업을 위해 조작하는 것을 간단하게 만들어 줍니다.
