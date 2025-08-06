console.log("loaded!")

let elementsToHighlight = [];

document.querySelectorAll('.cycle').forEach(el => {
    const cycleMembers = [...el.querySelectorAll("data").values()].map(v => v.value)
    el.querySelector('input[type="checkbox"]').addEventListener('change', e => {
        for (const member of cycleMembers) {
            document
                .querySelectorAll(`td data[value="${member}"]`)
                .forEach(cell => {
                    cell.classList.toggle("highlighted")
                })
        }
    })
})
