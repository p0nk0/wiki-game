let score = 0;
let traders = 0;
let taylors = 0;

const button = document.querySelector("#button");
const displayScore = document.querySelector("#profit");

const item1_text = document.querySelector("#item1_text");
const item1_cost = document.querySelector("#item1_cost");
const item1_button = document.querySelector("#item1_button");

const item2_text = document.querySelector("#item2_text");
const item2_cost = document.querySelector("#item2_cost");
const item2_button = document.querySelector("#item2_button");

button.onclick = () => {
    if (taylors == 0) {
        score += Math.round(1 + 1.5 ** traders);
        displayScore.textContent = `Profit: ${score}`;
    }
}

item1_button.onclick = () => {
    if (taylors > 0) {
        traders += 1;
        item1_text.textContent = `Traders: ${traders}`;
        item1_cost.textContent = `Cost: ${Math.round(10 + (2 ** (traders)))}`
    }
    else if (score >= Math.round(10 + (2 ** (traders)))) {
        score -= Math.round(10 + (2 ** (traders)));
        traders += 1;
        item1_text.textContent = `Traders: ${traders}`;
        item1_cost.textContent = `Cost: ${Math.round(10 + (2 ** (traders)))}`
    }
    displayScore.textContent = `Profit: ${score}`;
}

item2_button.onclick = () => {
    if (score >= 1000) {
        score = "Infinity";
        taylors += 1;
        item2_text.textContent = `You have hired Taylor Roberts, you have infinite money`;
        item2_cost.textContent = ``
        item2_button.textContent = ``
    }
    displayScore.textContent = `Profit: Infinite`;
}