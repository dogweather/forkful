---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:40.676701-07:00
description: "Jak to zrobi\u0107: W Google Apps Script manipulacja JSON jest procesem\
  \ prostym, g\u0142\xF3wnie dzi\u0119ki natywnej obs\u0142udze JavaScript dla parsowania\
  \ JSON i\u2026"
lastmod: '2024-03-13T22:44:34.926850-06:00'
model: gpt-4-0125-preview
summary: "W Google Apps Script manipulacja JSON jest procesem prostym, g\u0142\xF3\
  wnie dzi\u0119ki natywnej obs\u0142udze JavaScript dla parsowania JSON i konwertowania\
  \ na ci\u0105g znak\xF3w."
title: Praca z JSON
weight: 38
---

## Jak to zrobić:
W Google Apps Script manipulacja JSON jest procesem prostym, głównie dzięki natywnej obsłudze JavaScript dla parsowania JSON i konwertowania na ciąg znaków. Oto kilka typowych operacji:

**1. Parsowanie JSON**: Załóżmy, że pobieramy ciąg JSON z usługi sieciowej; parsowanie go na obiekt JavaScript jest niezbędne do manipulacji danymi.

```javascript
var jsonString = '{"name": "Przykładowy Projekt", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Wyjście: Przykładowy Projekt
```

**2. Konwertowanie obiektów JavaScript na ciąg JSON**: Odwrotnie, konwertowanie obiektu JavaScript na ciąg JSON jest użyteczne, kiedy musimy wysłać dane z Apps Script do zewnętrznej usługi.

```javascript
var projectData = {
  name: "Przykładowy Projekt",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Wyjście: '{"name":"Przykładowy Projekt","version":"1.0.0"}'
```

**3. Praca z danymi złożonymi**:
Dla bardziej złożonych struktur danych, takich jak tablice obiektów, proces pozostaje ten sam, pokazując elastyczność JSON dla reprezentacji danych.

```javascript
var projects = [
  {name: "Projekt 1", version: "1.0"},
  {name: "Projekt 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Wyjście: '[{"name":"Projekt 1","version":"1.0"},{"name":"Projekt 2","version":"2.0"}]'
```

## Szczegółowe omówienie
Wszechobecność JSON w nowoczesnych aplikacjach internetowych nie może być niedoceniona, zakorzeniona w jego prostocie i bezproblemowej integracji z JavaScript, językiem internetu. Jego projekt, inspirowany literałami obiektowymi JavaScript, choć bardziej rygorystyczny, ułatwia jego szybkie przyjęcie. Na początku lat 2000, JSON zyskał popularność jako alternatywa dla XML dla aplikacji internetowych napędzanych przez AJAX, oferując bardziej lekki i mniej rozwlekły format wymiany danych. Biorąc pod uwagę głęboką integrację Google Apps Script z różnymi usługami Google i zewnętrznymi, JSON pełni kluczową rolę w strukturyzacji, transporcie i manipulacji danymi na tych platformach.

Podczas gdy JSON dominuje w aplikacjach internetowych, istnieją alternatywne formaty danych, takie jak YAML dla plików konfiguracyjnych czy Protobuf dla bardziej wydajnej serializacji binarnej w środowiskach o wysokiej wydajności. Jednak równowaga JSON między czytelnością, łatwością użycia i szerokim wsparciem w różnych językach programowania i narzędziach umacnia jego pozycję jako wyboru domyślnego dla wielu programistów eksplorujących Google Apps Script i nie tylko.
