# DD1351-Logik-Lab2: Beviskontroll med Prolog

## 1. Introduktion 
I den här laborationen ska du konstruera och implementera en algoritm som
kontrollerar huruvida ett bevis skrivet i naturlig deduktion (som beskrivet i
kursboken) är korrekt eller inte. Indata till ditt program är en sekvent och
ett bevis, och programmet ska svara “yes” om det givna beviset är korrekt
och visar att den givna sekventen gäller och “no” annars. Som du känner till
utgörs naturlig deduktion av en uppsättning regler som beskriver när och
hur nya formler kan härledas. Ett språk väl lämpat för att kontrollera att
sådana regler följs är Prolog, vilket vi kommer använda i denna laboration.

## 2. Angreppssätt
Hur ett korrekt bevis i naturlig deduktion ska se ut defineras i figur 1. Ett
bevis kontrolleras alltså lämpligtvis rad för rad genom att verifiera att premisserna för respektive regel är uppfyllda. Vad man behöver ha tillgång till
för att avgöra om en regel är korrekt applicerad är vilka formler och boxar som härletts ovanför i beviset. Väljer du att gå igenom beviset uppifrån
och ner, måste du bokföra vilka formler och boxar som redan kontrollerats.
Väljer man att gå nerifrån och upp kan man finna informationen i den del
av beviset som man har kvar att gå igenom.

En stor del av arbetet går ut på att se till att boxar hanteras på ett korrekt sätt, dvs att man kan referera till hela boxar men inte till individuella
formler inuti en stängd box. Detta kan lösas på många sätt. En lösning är
att se till att en box öppnas vid och endast vid ett antagande och därefter,
temporärt, bortse från boxen tills alla rader i boxen kontrollerats. En annan lösning är att göra det rekursivt, genom att utnyttja det faktum att boxar
i sig måste uppfylla samma krav som ett komplett bevis.

> ### Ett bevis (eller härledning) till en sekvent är en formelsekvens:
> - där varje formel antingen är:
>   - en premiss i sekventen, eller
>   - ett tillfälligt antagande, eller
>   - resultatet av någon regel applicerad på
>     - formler som:
>       - kommer tidigare i sekvensen, och
>       - inte förekommer i någon avslutad box.
> - där alla antaganden är friade, och
> - som slutar i sekventens slutsats
> 
**Figur 1:** Definitionen av ett bevis i naturlig deduktion

Samtliga regler från figur 1.2 på sid. 27 i boken, samt regeln copy ska hanteras. Den fullständiga listan över regler hittar du i appendix A.

### 2.1 Exempel
Antag att vi vill kontrollera att beviset i figur 2 är ett korrekt bevis för följande sekvent: ¬¬(p → ¬p) ⊢ ¬p. Först kontrollerar vi att målet, dvs högerledet av sekventen, (i detta fall ¬p) står på sista raden i beviset. Därefter kontrollerar vi rad för rad som beskrivet i figur 3. I figuren låter vi understreck (“_”) stå för värden som vi inte bryr oss om, precis som i Prolog.

## 3. Uppgifter
Laborationen består av följande uppgifter:

1. Skriv ett icke-trivialt korrekt bevis och ett icke-trivialt felaktigt bevis, båda med boxar.
2. Torrkör din tänkta algoritm med penna och papper och visa hur den fungerar för dessa två bevis.
3. Implementera beviskontrolleringen i Prolog för bevis i formatet som definieras i appendix A.
4. Kör ditt program på bevisen som du hittat på själv och på alla fördefinierade testfall (se tips nedan). Notera att testsviten ej är uttömmande, så att alla tester passerar betyder inte att er lösning är korrekt.
5. Sammanställ resultaten och tillvägagångssättet i en rapport. Rapporten lämnas in och fungerar som underlag vid redovisningen. Rapporten ska vara strukturerad, välskriven, och heltäckande. Förutom en generell beskrivning av den valda beviskontrollalgoritmen och speciellt boxhanteringen, bör rapporten innehålla en tabell som listar namnen på era predikat samt när varje predikat är sant respektive falskt. Inkludera även programkoden och exempelbevisen i ett appendix.
6. Under redovisningen ska du kunna argumentera att lösningen är korrekt, och vara beredd på att besvara frågor om er lösning och följande frågor: skulle ditt beviskontrolleringsprogram kunna användas för att generera bevis för sekventer? Vad gäller för begränsade fall? Testa gärna att exekvera ert program med premisser och mål, men lämna beviset som en variabel, och se vad som händer.
